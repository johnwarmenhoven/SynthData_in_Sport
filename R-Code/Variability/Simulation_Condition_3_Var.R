
## Simulation Condition Three ##################################################

# Load libraries

library(readxl)
library(dplyr)
library(synthpop)
library(ggplot2)
library(cowplot)
library(pbmcapply)
library(ggplot2)
library(DescTools)
library(pROC)
library(cvGEE)
library(patchwork)
library(ggnewscale)
library(tidyr)
library(cowplot)
library(gridExtra)
library(grid)
library(patchwork)
library(dplyr)
library(gt)
library(knitr)
library(kableExtra)

## Raw data ####################################################################

# Read in the raw data, and make sure that PlayerID and Injury are factors to be 
# read by synthpop. WeekID kept as numeric to allow for easier computation when
# using CART models. 

################################################################################

df <- read_excel("Workload_injury_dataset x SD.xlsx", sheet = 2)

head(df)

df$PlayerID <- as.factor(df$PlayerID)
df$Injury <- as.factor(df$Injury)

## Making lagged variables #####################################################

# Thes will form other predictors that can be used in the generation of specific 
# variables when making synthetic data.

# Create two new variables to store the one- and two-week-lagged AcuteLoad values
df$AcuteLoadLag1 <- NA
df$AcuteLoadLag2 <- NA
df$AcuteLoadLag3 <- NA
df$ChronicLoadLag1 <- NA
df$ChronicLoadLag2 <- NA
df$ChronicLoadLag3 <- NA

# Loop over each player's data to fill in the new variables
for (player in unique(df$PlayerID)) {
  
  # Select the player's data
  player_data <- subset(df, PlayerID == player)
  
  # Sort the data by WeekID in case it isn't already sorted
  player_data <- player_data[order(player_data$WeekID), ]
  
  # Calculate the lagged Load values
  player_data$AcuteLoadLag1[2:nrow(player_data)] <- player_data$AcuteLoad[1:(nrow(player_data)-1)]
  player_data$AcuteLoadLag2[3:nrow(player_data)] <- player_data$AcuteLoad[1:(nrow(player_data)-2)]
  player_data$AcuteLoadLag3[4:nrow(player_data)] <- player_data$AcuteLoad[1:(nrow(player_data)-3)]
  
  player_data$ChronicLoadLag1[2:nrow(player_data)] <- player_data$ChronicLoad[1:(nrow(player_data)-1)]
  player_data$ChronicLoadLag2[3:nrow(player_data)] <- player_data$ChronicLoad[1:(nrow(player_data)-2)]
  player_data$ChronicLoadLag3[4:nrow(player_data)] <- player_data$ChronicLoad[1:(nrow(player_data)-3)]
  
  # Assign the lagged values back to the original dataframe
  df[df$PlayerID == player, "AcuteLoadLag1"] <- player_data$AcuteLoadLag1
  df[df$PlayerID == player, "AcuteLoadLag2"] <- player_data$AcuteLoadLag2
  df[df$PlayerID == player, "AcuteLoadLag3"] <- player_data$AcuteLoadLag3
  
  df[df$PlayerID == player, "ChronicLoadLag1"] <- player_data$ChronicLoadLag1
  df[df$PlayerID == player, "ChronicLoadLag2"] <- player_data$ChronicLoadLag2
  df[df$PlayerID == player, "ChronicLoadLag3"] <- player_data$ChronicLoadLag3
  
}

################################################################################

## Original GEE Model

injury <- readxl::read_excel("Workload_injury_dataset x SD.xlsx")
injury_trimmed <-
  na.omit(injury[, c("Injury",
                     "ACWR4wks",
                     "ACWR3wks",
                     "ACWR2wks",
                     "PlayerID",
                     "AcuteLoad",
                     "ChronicLoad")])

# Main model with 4 week chronic
true_model <- geepack::geeglm(
  Injury ~ ACWR4wks,
  id = PlayerID,
  data = injury_trimmed,
  corstr = "exchangeable",
  family = binomial
)
summary(true_model)
final <- broom::tidy(true_model, 
                     conf.int = T, 
                     exp = F)

################################################################################

# Create a dataset getting rid of the first 3 points per person.

# This is to compare outcomes from synthetic data, where chronic is derived
# from the synthetic acute variable and we can't use the first 3 points per 
# person. This creates a separate base GEE model, which can be used to compare 
# synthetic data sets with those points removed. 

## GEE Model on trimmed dataset ################################################

injury_minus <- injury %>%
  group_by(PlayerID) %>%
  mutate(ChronicLoad_calc = ifelse(WeekID <= 3, NA, (lag(AcuteLoad, 1, default = 0) + lag(AcuteLoad, 2, default = 0) + lag(AcuteLoad, 3, default = 0) + AcuteLoad) / 4))

injury_trimmed_minus <-
  na.omit(injury_minus[, c("Injury",
                     "ACWR4wks",
                     "ACWR3wks",
                     "ACWR2wks",
                     "PlayerID",
                     "AcuteLoad",
                     "ChronicLoad",
                     "ChronicLoad_calc")])

# Main model with 4 week chronic
true_model_minus <- geepack::geeglm(
  Injury ~ ACWR4wks,
  id = PlayerID,
  data = injury_trimmed_minus,
  corstr = "exchangeable",
  family = binomial
)
summary(true_model_minus)
final_minus <- broom::tidy(true_model_minus, 
                     conf.int = T, 
                     exp = F)

################################################################################

### Main Loop ##################################################################

# Initialize the dataframes to store results from each iteration
results_df_ex3_nCW <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_df_ex3_nCW) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
results_df_ex3_nCW_s <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_df_ex3_nCW_s) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")

player1_acute_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_acute_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_acute_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_acute_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_acute_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

player1_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

player1_s_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_s_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_s_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_s_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_s_chronic_load_ex3_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

pMSE_ex3_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(pMSE_ex3_nCW) <- c("pMSE")
s_pMSE_ex3_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(s_pMSE_ex3_nCW) <- c("s_pMSE")
PO50_ex3_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(PO50_ex3_nCW) <- c("PO50")
time.elapsed_ex3_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(time.elapsed_ex3_nCW) <- c("Time")

################################################################################

## The main for-loop ###########################################################

# This loop creates synthetic data sets, and also calculated metrics that can be
# used to compare the synthetic data to the original data. This is similar to 
# the loop in the normal code for simulation conditions. 

# Loop 500 times
for (i in 1:500) {
  
  # Set a new seed each time
  myseed <- 20200525 + i
  
  start.time <- Sys.time()
  
  # Generate the synthetic dataset
  synth.obj <- syn(df, 
                   visit.sequence = c(5, 2, 6, 9, 3, 4),
                   method = c("","","cart","cart","","","","","","",""),
                   seed = myseed)
  Data <- synth.obj$syn
  
  end.time <- Sys.time()
  
  time.elapsed_ex3_nCW[i,] <- end.time - start.time
  time.elapsed_ex3_nCW[i,]
  
  Data <- Data %>%
    group_by(PlayerID) %>%
    mutate(ChronicLoad_syn = ifelse(WeekID <= 3, NA, (lag(AcuteLoad, 1, default = 0) + lag(AcuteLoad, 2, default = 0) + lag(AcuteLoad, 3, default = 0) + AcuteLoad) / 4))
  
  # Subset the data for participant 1, 4, 7 and 11
  player1_data <- subset(Data, PlayerID == 1)
  player4_data <- subset(Data, PlayerID == 4)
  player7_data <- subset(Data, PlayerID == 7)
  player11_data <- subset(Data, PlayerID == 11)
  player15_data <- subset(Data, PlayerID == 15)
  
  # Extract the AcuteLoad variable
  player1_acute_load_ex3_nCW[i,] <- player1_data$AcuteLoad
  player4_acute_load_ex3_nCW[i,] <- player4_data$AcuteLoad
  player7_acute_load_ex3_nCW[i,] <- player7_data$AcuteLoad
  player11_acute_load_ex3_nCW[i,] <- player11_data$AcuteLoad
  player15_acute_load_ex3_nCW[i,] <- player15_data$AcuteLoad
  
  # Extract the ChronLoad variable
  player1_chronic_load_ex3_nCW[i,] <- player1_data$ChronicLoad
  player4_chronic_load_ex3_nCW[i,] <- player4_data$ChronicLoad
  player7_chronic_load_ex3_nCW[i,] <- player7_data$ChronicLoad
  player11_chronic_load_ex3_nCW[i,] <- player11_data$ChronicLoad
  player15_chronic_load_ex3_nCW[i,] <- player15_data$ChronicLoad
  
  # Extract the Synthetically derived ChronLoad variables
  player1_s_chronic_load_ex3_nCW[i,] <- player1_data$ChronicLoad_syn
  player4_s_chronic_load_ex3_nCW[i,] <- player4_data$ChronicLoad_syn
  player7_s_chronic_load_ex3_nCW[i,] <- player7_data$ChronicLoad_syn
  player11_s_chronic_load_ex3_nCW[i,] <- player11_data$ChronicLoad_syn
  player15_s_chronic_load_ex3_nCW[i,] <- player15_data$ChronicLoad_syn
  
  # Global measures
  
  # I have included both the Acute and Chronic synthetic variables in the 
  # overall calculation of global utility.  
  
  u1 <- utility.gen(Data, df, method = "logit", 
                    vars = c("AcuteLoad", "ChronicLoad"))
  
  pMSE_ex3_nCW[i,] <- u1$pMSE
  s_pMSE_ex3_nCW[i,] <- u1$S_pMSE
  PO50_ex3_nCW[i,] <- u1$PO50
  
  #### First Model #############################################################
  
  # This is applied to all of the data, and includes simulating new chronic variables
  # independent of acute variables. This isn't best practice (as they are linked to each other)
  # but is important to inspect to see if positive results here could be misleading.
  
  # Process the data
  Data$ACWR4wks <- Data$AcuteLoad/Data$ChronicLoad
  Data$Injury <- as.numeric(Data$Injury)
  Data$PlayerID <- as.numeric(Data$PlayerID)
  Data$Injury <- ifelse(Data$Injury == 1, 0, 1)
  data_trimmed <- na.omit(Data[, c("Injury", "ACWR4wks", "PlayerID", "AcuteLoad", "ChronicLoad")])
  
  # Fit the GEE model
  syn_model <- geepack::geeglm(Injury ~ ACWR4wks, id = PlayerID, data = data_trimmed, corstr = "exchangeable", family = binomial)
  final_Syn_T <- broom::tidy(syn_model, conf.int = T, exp = F)
  
  # Store the result of the second row in the results_df dataframe
  results_df_ex3_nCW[i, ] <- final_Syn_T[2, ]
  
  ##############################################################################
  
  #### Second Model ############################################################
  
  # This time we manually derive the chronic load data from the synthetically derived
  # Acute data. So the calculation of chronic load is linked to the synthetic acute
  # data. Better practice, but I suspect it will be more innacurate, as noise will
  # be carried forward from the acute load. 
  
  # Process the data
  Data$ACWR4wks_s <- Data$AcuteLoad/Data$ChronicLoad_syn
  data_trimmed_s <- na.omit(Data[, c("Injury", "ACWR4wks_s", "PlayerID", "AcuteLoad", "ChronicLoad", "ChronicLoad_syn", "WeekID")])
  
  # Fit the GEE model
  syn_model_s <- geepack::geeglm(Injury ~ ACWR4wks_s, id = PlayerID, data = data_trimmed_s, corstr = "exchangeable", family = binomial)
  final_Syn_Ts <- broom::tidy(syn_model_s, conf.int = T, exp = F)
  
  # Store the result of the second row in the results_df dataframe
  results_df_ex3_nCW_s[i, ] <- final_Syn_Ts[2, ]
  
  #### END OF LOOP
  
}

################################################################################

### Calculate absolute differences between successive pairs of observations
# for the GEE p-value, estimate and std.error. 

# Create the new dataframe with the differences in absolute values
results_df_ex3_nCW <- results_df_ex3_nCW %>%
  mutate(p.value.error = c(NA, abs(diff(p.value)))) %>%
  mutate(estimate.error = c(NA, abs(diff(estimate)))) %>%
  mutate(std.error.error = c(NA, abs(diff(std.error))))


# Create the new dataframe with the differences in absolute values
results_df_ex3_nCW_s <- results_df_ex3_nCW_s %>%
  mutate(p.value.error = c(NA, abs(diff(p.value)))) %>%
  mutate(estimate.error = c(NA, abs(diff(estimate)))) %>%
  mutate(std.error.error = c(NA, abs(diff(std.error))))

################################################################################

# Second loop acute and chronic data. The absolute errors for these are 
# calculated between successive pairs of of observations (i.e. successive pairs
# of synthetic datasets). 

AcuteLoad_RMSE_ex3_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(AcuteLoad_RMSE_ex3_nCW) <- c("RMSE")
ChronicLoad_RMSE_ex3_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(ChronicLoad_RMSE_ex3_nCW) <- c("RMSE")
ChronicLoad_RMSE_ex3_nCW_s <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(ChronicLoad_RMSE_ex3_nCW_s) <- c("RMSE")

################################################################################

## SECOND LOOP

# Initialize a variable to store the previous synthetic dataset
prev_synthetic <- NULL

# Loop 500 times
for (i in 1:500) {
  
  # Set a new seed each time
  myseed <- 20200525 + i
  
  start.time <- Sys.time()
  
  # Generate the synthetic dataset
  synth.obj <- syn(df, 
                   visit.sequence = c(5, 2, 6, 9, 3, 4),
                   method = c("","","cart","cart","","","","","","",""),
                   seed = myseed)
  Data <- synth.obj$syn
  
  # Check if it's not the first iteration
  if (!is.null(prev_synthetic)) {
    
    Data <- Data %>%
      group_by(PlayerID) %>%
      mutate(ChronicLoad_syn = ifelse(WeekID <= 3, NA, (lag(AcuteLoad, 1, default = 0) + lag(AcuteLoad, 2, default = 0) + lag(AcuteLoad, 3, default = 0) + AcuteLoad) / 4))
    
    # Calculate the error between the current and previous synthetic datasets
    # for acute load. 
    diff <- prev_synthetic$AcuteLoad - Data$AcuteLoad
    diff <- na.omit(diff)
    absl <- abs(diff)
    AcuteLoad_RMSE_ex3_nCW[i, ] <- mean(absl)
    
    # Calculate the error between the current and previous synthetic datasets
    # for acute load (scenario one).
    diff <- prev_synthetic$ChronicLoad - Data$ChronicLoad
    diff <- na.omit(diff)
    absl <- abs(diff)
    ChronicLoad_RMSE_ex3_nCW[i, ] <- mean(absl)
    
    # Calculate the error between the current and previous synthetic datasets
    # for chronic load (scenario two).
    diff <- df$ChronicLoad - Data$ChronicLoad_syn 
    diff <- na.omit(diff)
    absl <- abs(diff)
    ChronicLoad_RMSE_ex3_nCW_s[i, ] <- mean(absl)
    
  }
  
  # Store the current synthetic dataset for the next iteration
  prev_synthetic <- Data
  
## END LOOP
  
}

################################################################################

#### Performance numeric results

# Here we continue making some of the metrics to be summarized in a table, which
# will follow. 

# Add std.error residuals. Earlier for the plots we made residuals for the GEE
# estimate, upper and lower bound of CIs. We did not make residuals for the SE
# comparing synthetic data to the original data. That takes place below. 

pval.error <- data.frame("p.value.error" = results_df_ex3_nCW$p.value.error) 
pval.error_s <- data.frame("p.value.error_s" = results_df_ex3_nCW_s$p.value.error) 

estimate.error <- data.frame("estimate.error" = results_df_ex3_nCW$estimate.error) 
estimate.error_s <- data.frame("estimate.error_s" = results_df_ex3_nCW_s$estimate.error) 

std.error.error <- data.frame("std.error.error" = results_df_ex3_nCW$std.error.error)
std.error.error_s <- data.frame("std.error.error_s" = results_df_ex3_nCW_s$std.error.error) 

#### CREATING A SUMMARISED TABLE OF RESULTS ####################################

## The following looks at creating a summarized results table, which will be 
# exported. 

# create a list of data frames of metrics to be included.
dfs <- list(AcuteLoad_RMSE_ex3_nCW, 
            ChronicLoad_RMSE_ex3_nCW, 
            ChronicLoad_RMSE_ex3_nCW_s,
            pval.error,
            pval.error_s,
            estimate.error,
            estimate.error_s,
            std.error.error,
            std.error.error_s)

# calculate summary statistics for each data frame
results <- lapply(dfs, function(df) {
  # exclude the second column if it exists
  if (ncol(df) == 2) {
    df <- select(df, -2)
  }
  
  # calculate summary statistics
  means <- colMeans(df, na.rm = TRUE)
  sds <- apply(df, 2, sd, na.rm = TRUE)
  lower_cis <- means - 1.96 * sds / sqrt(nrow(df))
  upper_cis <- means + 1.96 * sds / sqrt(nrow(df))
  mins <- apply(df, 2, min, na.rm = TRUE)
  maxs <- apply(df, 2, max, na.rm = TRUE)
  
  # combine the results into a data frame
  data.frame(Mean = means, SD = sds, "LowerCI" = lower_cis, "UpperCI" = upper_cis, Min = mins, Max = maxs)
})

# combine the results into a single data frame and set column names
table <- bind_rows(results, .id = "Variable") %>%
  pivot_longer(-Variable, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  as.data.frame() # convert to dataframe

## For the column names here, any variable with "(d)" after it, relates 
# specifically to a context where the metrics have come from scenario 2 for 
# calculating chronic load, where it was derived from acute synthetic load. 

col_names <- c("AcuteLoad RMSE", "ChronicLoad RMSE", "ChronicLoad RMSE (d)",
               "pval.error", "pval.error (d)", "estimate.error", "estimate.error (d)",
               "std.error.abs", "std.error.abs (d)")
colnames(table)[-1] <- col_names # set column names, excluding first column

# format numerical values to two decimal places
table[, 2:10] <- formatC(as.matrix(table[, 2:10]), format = "f", digits = 2)

table

library("writexl")
write_xlsx(table,"C:\\Users\\159479\\OneDrive - UTS\\Meta-Research\\2. Synthetic Data\\Experimental Paper\\1. Completed Article & Code\\Variability\\Results_SC3_Var.xlsx")

