
## Simulation Condition Two ####################################################

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

## Original GEE Model ##########################################################

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

results_df_ex2_nCW <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_df_ex2_nCW) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
results_df_ex2_nCW_s <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_df_ex2_nCW_s) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")

player1_acute_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_acute_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_acute_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_acute_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_acute_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

player1_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

player1_s_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_s_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_s_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_s_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_s_chronic_load_ex2_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

pMSE_ex2_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(pMSE_ex2_nCW) <- c("pMSE")
s_pMSE_ex2_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(s_pMSE_ex2_nCW) <- c("s_pMSE")
PO50_ex2_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(PO50_ex2_nCW) <- c("PO50")
time.elapsed_ex2_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(time.elapsed_ex2_nCW) <- c("Time")

AcuteLoad_RMSE_ex2_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(AcuteLoad_RMSE_ex2_nCW) <- c("RMSE")
ChronicLoad_RMSE_ex2_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(ChronicLoad_RMSE_ex2_nCW) <- c("RMSE")
ChronicLoad_RMSE_ex2_nCW_s <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(ChronicLoad_RMSE_ex2_nCW_s) <- c("RMSE")

################################################################################

## The main for-loop ###########################################################

# This loop creates synthetic data sets, and also calculated metrics that can be
# used to compare the synthetic data to the original data.

# Loop 500 times
for (i in 1:500) {
  
  # Set a new seed each time
  myseed <- 20200525 + i
  
  start.time <- Sys.time()
  
  # Generate the synthetic dataset
  synth.obj <- syn(df, 
                   visit.sequence = c(5, 2, 1, 3, 4),
                   method = c("","","cart","cart",""),
                   seed = myseed)
  Data <- synth.obj$syn
  
  end.time <- Sys.time()
  
  time.elapsed_ex2_nCW[i,] <- end.time - start.time
  time.elapsed_ex2_nCW[i,]
  
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
  player1_acute_load_ex2_nCW[i,] <- player1_data$AcuteLoad
  player4_acute_load_ex2_nCW[i,] <- player4_data$AcuteLoad
  player7_acute_load_ex2_nCW[i,] <- player7_data$AcuteLoad
  player11_acute_load_ex2_nCW[i,] <- player11_data$AcuteLoad
  player15_acute_load_ex2_nCW[i,] <- player15_data$AcuteLoad
  
  # Extract the ChronLoad variable
  player1_chronic_load_ex2_nCW[i,] <- player1_data$ChronicLoad
  player4_chronic_load_ex2_nCW[i,] <- player4_data$ChronicLoad
  player7_chronic_load_ex2_nCW[i,] <- player7_data$ChronicLoad
  player11_chronic_load_ex2_nCW[i,] <- player11_data$ChronicLoad
  player15_chronic_load_ex2_nCW[i,] <- player15_data$ChronicLoad
  
  # Extract the Synthetically derived ChronLoad variables
  player1_s_chronic_load_ex2_nCW[i,] <- player1_data$ChronicLoad_syn
  player4_s_chronic_load_ex2_nCW[i,] <- player4_data$ChronicLoad_syn
  player7_s_chronic_load_ex2_nCW[i,] <- player7_data$ChronicLoad_syn
  player11_s_chronic_load_ex2_nCW[i,] <- player11_data$ChronicLoad_syn
  player15_s_chronic_load_ex2_nCW[i,] <- player15_data$ChronicLoad_syn
  
  # Global measures
  
  # I have included both the Acute and Chronic synthetic variables in the 
  # overall calculation of global utility.  
  
  u1 <- utility.gen(Data, df, method = "logit", 
                    vars = c("AcuteLoad", "ChronicLoad"))
  
  pMSE_ex2_nCW[i,] <- u1$pMSE
  s_pMSE_ex2_nCW[i,] <- u1$S_pMSE
  PO50_ex2_nCW[i,] <- u1$PO50
  
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
  results_df_ex2_nCW[i, ] <- final_Syn_T[2, ]
  
  ### Accuracy of acute load fit
  diff <- df$AcuteLoad - Data$AcuteLoad 
  diff <- na.omit(diff)
  absl <- abs(diff)
  AcuteLoad_RMSE_ex2_nCW[i, ] <- mean(absl)
  
  ### Accuracy of chronic load fit (scenario one)
  diff <- df$ChronicLoad - Data$ChronicLoad 
  diff <- na.omit(diff)
  absl <- abs(diff)
  ChronicLoad_RMSE_ex2_nCW[i, ] <- mean(absl)
  
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
  results_df_ex2_nCW_s[i, ] <- final_Syn_Ts[2, ]
  
  ### Accuracy of load fit (only for the new Chronic)

  diff <- df$ChronicLoad - Data$ChronicLoad_syn 
  diff <- na.omit(diff)
  absl <- abs(diff)
  ChronicLoad_RMSE_ex2_nCW_s[i, ] <- mean(absl)
  
  #### END OF LOOP
}

################################################################################

#### PRELIMINARY RESULTS #######################################################

## The following section provides some new metrics will will be used for both
# the graphs which will follow, as well as summarised tables of the results at
# the end of this r-code. 

## RESIDUALS FROM GEE (for plotting)

# Creating residuals, subtracting GEE outcomes from the original analysis from
# the same outcomes from the analysis on the synthetic datasets.

# This was done for the GEE estimate and upper and lower CIs for plotting.
# The estimate will be used in the results table at the end of the r-code. 
results_df_ex2_nCW$conf.low.resid <- results_df_ex2_nCW$conf.low - t(as.data.frame(rep(final[2,6],nrow(results_df_ex2_nCW))))
results_df_ex2_nCW_s$conf.low.resid <- results_df_ex2_nCW_s$conf.low - t(as.data.frame(rep(final_minus[2,6],nrow(results_df_ex2_nCW_s))))

results_df_ex2_nCW$conf.high.resid <- results_df_ex2_nCW$conf.high - t(as.data.frame(rep(final[2,7],nrow(results_df_ex2_nCW))))
results_df_ex2_nCW_s$conf.high.resid <- results_df_ex2_nCW_s$conf.high - t(as.data.frame(rep(final_minus[2,7],nrow(results_df_ex2_nCW_s))))

results_df_ex2_nCW$estimate.resid <- results_df_ex2_nCW$estimate - t(as.data.frame(rep(final[2,2],nrow(results_df_ex2_nCW))))
results_df_ex2_nCW_s$estimate.resid <- results_df_ex2_nCW_s$estimate - t(as.data.frame(rep(final_minus[2,2],nrow(results_df_ex2_nCW_s))))

## Creating long dataframes for plotting.
results_df_long_ex2_nCW <- gather(results_df_ex2_nCW, key = "variable", value = "value", estimate, conf.low, conf.high)
results_df_long_ex2_nCW_s <- gather(results_df_ex2_nCW_s, key = "variable", value = "value", estimate, conf.low, conf.high)
final_long <- gather(final, key = "variable", value = "value", estimate, conf.low, conf.high)
final_long_minus <- gather(final_minus, key = "variable", value = "value", estimate, conf.low, conf.high)

## Creating metric for proportion of p-values less than 0.05 for the ACWR GEE coefficient. 
# This is only for plotting.
p_below_05_ex2_nCW <- sum(results_df_ex2_nCW$p.value < 0.05) / nrow(results_df_ex2_nCW) * 100
p_below_05_ex2_nCW_s <- sum(results_df_ex2_nCW_s$p.value < 0.05) / nrow(results_df_ex2_nCW_s) * 100

## Creating p-value residuals by subtracting original analysis p-value from the 
# p-value in each synthetic dataset. These will be used in results tables as well.
p_below_05_err_ex2_nCW <- sqrt((results_df_ex2_nCW$p.value-final$p.value)^2)
p_below_05_err_ex2_nCW <- as.data.frame(p_below_05_err_ex2_nCW)

p_below_05_err_ex2_nCW_s <- sqrt((results_df_ex2_nCW_s$p.value-final$p.value)^2)
p_below_05_err_ex2_nCW_s <- as.data.frame(p_below_05_err_ex2_nCW_s)

################################################################################

#### Performance numeric results

# Here we continue making some of the metrics to be summarized in a table, which
# will follow. 

# Add std.error residuals. Earlier for the plots we made residuals for the GEE
# estimate, upper and lower bound of CIs. We did not make residuals for the SE
# comparing synthetic data to the original data. That takes place below. 

results_df_ex2_nCW$std.error.resid <- results_df_ex2_nCW$std.error - t(as.data.frame(rep(final[2,3],nrow(results_df_ex2_nCW))))
results_df_ex2_nCW_s$std.error.resid <- results_df_ex2_nCW_s$std.error - t(as.data.frame(rep(final_minus[2,3],nrow(results_df_ex2_nCW_s))))

#### MAE of GEE Outcomes. ######################################################

## Below we turn the residuals into absolute errors. Later in the table we 
# calculate the mean of these to get the MAE for all metrics below. 

conf.low.abs <- abs(data.frame("conf.low.resid" = results_df_ex2_nCW$conf.low.resid)) 
conf.low.abs_s <- abs(data.frame("conf.low.resid_s" = results_df_ex2_nCW_s$conf.low.resid)) 

conf.high.abs <- abs(data.frame("conf.high.resid" = results_df_ex2_nCW$conf.high.resid)) 
conf.high.abs_s <- abs(data.frame("conf.high.resid_s" = results_df_ex2_nCW_s$conf.high.resid))

estimate.abs <- abs(data.frame("estimate.resid" = results_df_ex2_nCW$estimate.resid)) 
estimate.abs_s <- abs(data.frame("estimate.resid_s" = results_df_ex2_nCW_s$estimate.resid)) 

std.error.abs <- abs(data.frame("std.error.resid" = results_df_ex2_nCW$std.error.resid))
std.error.abs_s <- abs(data.frame("std.error.resid_s" = results_df_ex2_nCW_s$std.error.resid)) 

#### CREATING A SUMMARISED TABLE OF RESULTS ####################################

## The following looks at creating a summarized results table, which will be 
# exported. 

# create a list of data frames of metrics to be included.
dfs <- list(pMSE_ex2_nCW, 
            s_pMSE_ex2_nCW, 
            PO50_ex2_nCW, 
            time.elapsed_ex2_nCW, 
            AcuteLoad_RMSE_ex2_nCW, 
            ChronicLoad_RMSE_ex2_nCW, 
            ChronicLoad_RMSE_ex2_nCW_s,
            conf.low.abs,
            conf.low.abs_s,
            conf.high.abs,
            conf.high.abs_s,
            estimate.abs,
            estimate.abs_s,
            std.error.abs,
            std.error.abs_s,
            p_below_05_err_ex2_nCW,
            p_below_05_err_ex2_nCW_s)

# calculate summary statistics for each data frame
results <- lapply(dfs, function(df) {
  # exclude the second column if it exists
  if (ncol(df) == 2) {
    df <- select(df, -2)
  }
  
  # calculate summary statistics
  means <- colMeans(df, na.rm = TRUE)
  sds <- apply(df, 2, sd, na.rm = TRUE)
  
  # combine the results into a data frame
  data.frame(Mean = means, SD = sds)
})

# combine the results into a single data frame and set column names
table <- bind_rows(results, .id = "Variable") %>%
  pivot_longer(-Variable, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  as.data.frame() # convert to dataframe

## For the column names here, any variable with "(d)" after it, relates 
# specifically to a context where the metrics have come from scenario 2 for 
# calculating chronic load, where it was derived from acute synthetic load. 

col_names <- c("pMSE", "s-pMSE", "PO50", "Time (s)", "AcuteLoad RMSE", "ChronicLoad RMSE", "ChronicLoad RMSE (d)",
               "Conf.Low.abs", "Conf.Low.abs (d)", "Conf.High.abs", "Conf.High.abs (d)",
               "Estimate.abs", "Estimate.abs (d)", "Std.Error.abs", "Std.Error.abs (d)",
               "p.below.05.err", "p.below.05.err (d)")
colnames(table)[-1] <- col_names # set column names, excluding first column

# format numerical values to two decimal places
table[, 2:18] <- formatC(as.matrix(table[, 2:18]), format = "f", digits = 2)

table

library("writexl")
write_xlsx(table,"C:\\Users\\159479\\OneDrive - UTS\\Meta-Research\\2. Synthetic Data\\Experimental Paper\\1. Completed Article & Code\\Results_SC2.xlsx")

################################################################################
