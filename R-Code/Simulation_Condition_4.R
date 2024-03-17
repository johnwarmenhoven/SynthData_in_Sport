
## Simulation Condition Four ##################################################

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

results_df_ex4_nCW <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_df_ex4_nCW) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")
results_df_ex4_nCW_s <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(results_df_ex4_nCW_s) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")

player1_acute_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_acute_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_acute_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_acute_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_acute_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

player1_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

player1_s_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 1), nrow = 0))
player4_s_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 4), nrow = 0))
player7_s_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 7), nrow = 0))
player11_s_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 11), nrow = 0))
player15_s_chronic_load_ex4_nCW <- data.frame(matrix(ncol = sum(df$PlayerID == 15), nrow = 0))

pMSE_ex4_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(pMSE_ex4_nCW) <- c("pMSE")
s_pMSE_ex4_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(s_pMSE_ex4_nCW) <- c("s_pMSE")
PO50_ex4_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(PO50_ex4_nCW) <- c("PO50")
time.elapsed_ex4_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(time.elapsed_ex4_nCW) <- c("Time")

AcuteLoad_RMSE_ex4_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(AcuteLoad_RMSE_ex4_nCW) <- c("RMSE")
ChronicLoad_RMSE_ex4_nCW <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(ChronicLoad_RMSE_ex4_nCW) <- c("RMSE")
ChronicLoad_RMSE_ex4_nCW_s <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(ChronicLoad_RMSE_ex4_nCW_s) <- c("RMSE")

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
                   visit.sequence = c(5, 2, 6, 7, 8, 9, 10, 11, 3, 4),
                   method = c("","","cart","cart","","","","","","",""),
                   seed = myseed)
  Data <- synth.obj$syn
  
  end.time <- Sys.time()
  
  time.elapsed_ex4_nCW[i,] <- end.time - start.time
  time.elapsed_ex4_nCW[i,]
  
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
  player1_acute_load_ex4_nCW[i,] <- player1_data$AcuteLoad
  player4_acute_load_ex4_nCW[i,] <- player4_data$AcuteLoad
  player7_acute_load_ex4_nCW[i,] <- player7_data$AcuteLoad
  player11_acute_load_ex4_nCW[i,] <- player11_data$AcuteLoad
  player15_acute_load_ex4_nCW[i,] <- player15_data$AcuteLoad
  
  # Extract the ChronLoad variable
  player1_chronic_load_ex4_nCW[i,] <- player1_data$ChronicLoad
  player4_chronic_load_ex4_nCW[i,] <- player4_data$ChronicLoad
  player7_chronic_load_ex4_nCW[i,] <- player7_data$ChronicLoad
  player11_chronic_load_ex4_nCW[i,] <- player11_data$ChronicLoad
  player15_chronic_load_ex4_nCW[i,] <- player15_data$ChronicLoad
  
  # Extract the Synthetically derived ChronLoad variables
  player1_s_chronic_load_ex4_nCW[i,] <- player1_data$ChronicLoad_syn
  player4_s_chronic_load_ex4_nCW[i,] <- player4_data$ChronicLoad_syn
  player7_s_chronic_load_ex4_nCW[i,] <- player7_data$ChronicLoad_syn
  player11_s_chronic_load_ex4_nCW[i,] <- player11_data$ChronicLoad_syn
  player15_s_chronic_load_ex4_nCW[i,] <- player15_data$ChronicLoad_syn
  
  # Global measures
  
  # I have included both the Acute and Chronic synthetic variables in the 
  # overall calculation of global utility.  
  
  u1 <- utility.gen(Data, df, method = "logit", 
                    vars = c("AcuteLoad", "ChronicLoad"))
  
  pMSE_ex4_nCW[i,] <- u1$pMSE
  s_pMSE_ex4_nCW[i,] <- u1$S_pMSE
  PO50_ex4_nCW[i,] <- u1$PO50
  
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
  results_df_ex4_nCW[i, ] <- final_Syn_T[2, ]
  
  ### Accuracy of acute load fit
  diff <- df$AcuteLoad - Data$AcuteLoad 
  diff <- na.omit(diff)
  absl <- abs(diff)
  AcuteLoad_RMSE_ex4_nCW[i, ] <- mean(absl)
  
  ### Accuracy of chronic load fit (scenario one)
  diff <- df$ChronicLoad - Data$ChronicLoad 
  diff <- na.omit(diff)
  absl <- abs(diff)
  ChronicLoad_RMSE_ex4_nCW[i, ] <- mean(absl)
  
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
  results_df_ex4_nCW_s[i, ] <- final_Syn_Ts[2, ]
  
  ### Accuracy of load fit (only for the new Chronic)

  diff <- df$ChronicLoad - Data$ChronicLoad_syn 
  diff <- na.omit(diff)
  absl <- abs(diff)
  ChronicLoad_RMSE_ex4_nCW_s[i, ] <- mean(absl)
  
  #### END OF LOOP
}

################################################################################

#### PREPARING RESULTS & GRAPHS ################################################

# The following section is setting up for both graphing and results.

## Setting up components to go onto the graphs ################################# 

# Factor and numeric variables for plotting in ggplot2.
results_df_ex4_nCW$term <- as.factor(results_df_ex4_nCW$term)
results_df_ex4_nCW$term_numeric <- as.numeric(factor(results_df_ex4_nCW$term))

results_df_ex4_nCW_s$term <- as.factor(results_df_ex4_nCW_s$term)
results_df_ex4_nCW_s$term_numeric <- as.numeric(factor(results_df_ex4_nCW_s$term))

# Factor and numeric variables for plotting in ggplot2 for the original GEE model.
final$term_numeric <- as.numeric(factor(final$term == 1))
final_minus$term_numeric <- as.numeric(factor(final_minus$term == 1))

## Numeric variables for global utility.

PO50_ex4_nCW$term_numeric <- 1
pMSE_ex4_nCW$term_numeric <- 1
time.elapsed_ex4_nCW$term_numeric <- 1

#### PRELIMINARY RESULTS #######################################################

## The following section provides some new metrics will will be used for both
# the graphs which will follow, as well as summarised tables of the results at
# the end of this r-code. 

## RESIDUALS FROM GEE (for plotting)

# Creating residuals, subtracting GEE outcomes from the original analysis from
# the same outcomes from the analysis on the synthetic datasets.

# This was done for the GEE estimate and upper and lower CIs for plotting.
# The estimate will be used in the results table at the end of the r-code. 
results_df_ex4_nCW$conf.low.resid <- results_df_ex4_nCW$conf.low - t(as.data.frame(rep(final[2,6],nrow(results_df_ex4_nCW))))
results_df_ex4_nCW_s$conf.low.resid <- results_df_ex4_nCW_s$conf.low - t(as.data.frame(rep(final_minus[2,6],nrow(results_df_ex4_nCW_s))))

results_df_ex4_nCW$conf.high.resid <- results_df_ex4_nCW$conf.high - t(as.data.frame(rep(final[2,7],nrow(results_df_ex4_nCW))))
results_df_ex4_nCW_s$conf.high.resid <- results_df_ex4_nCW_s$conf.high - t(as.data.frame(rep(final_minus[2,7],nrow(results_df_ex4_nCW_s))))

results_df_ex4_nCW$estimate.resid <- results_df_ex4_nCW$estimate - t(as.data.frame(rep(final[2,2],nrow(results_df_ex4_nCW))))
results_df_ex4_nCW_s$estimate.resid <- results_df_ex4_nCW_s$estimate - t(as.data.frame(rep(final_minus[2,2],nrow(results_df_ex4_nCW_s))))

## Creating long dataframes for plotting.
results_df_long_ex4_nCW <- gather(results_df_ex4_nCW, key = "variable", value = "value", estimate, conf.low, conf.high)
results_df_long_ex4_nCW_s <- gather(results_df_ex4_nCW_s, key = "variable", value = "value", estimate, conf.low, conf.high)
final_long <- gather(final, key = "variable", value = "value", estimate, conf.low, conf.high)
final_long_minus <- gather(final_minus, key = "variable", value = "value", estimate, conf.low, conf.high)

## Creating metric for proportion of p-values less than 0.05 for the ACWR GEE coefficient. 
# This is only for plotting.
p_below_05_ex4_nCW <- sum(results_df_ex4_nCW$p.value < 0.05) / nrow(results_df_ex4_nCW) * 100
p_below_05_ex4_nCW_s <- sum(results_df_ex4_nCW_s$p.value < 0.05) / nrow(results_df_ex4_nCW_s) * 100

## Creating p-value residuals by subtracting original analysis p-value from the 
# p-value in each synthetic dataset. These will be used in results tables as well.
p_below_05_err_ex4_nCW <- sqrt((results_df_ex4_nCW$p.value-final$p.value)^2)
p_below_05_err_ex4_nCW <- as.data.frame(p_below_05_err_ex4_nCW)

p_below_05_err_ex4_nCW_s <- sqrt((results_df_ex4_nCW_s$p.value-final$p.value)^2)
p_below_05_err_ex4_nCW_s <- as.data.frame(p_below_05_err_ex4_nCW_s)

################################################################################

#### GLOBAL UTILITY PERFORMANCE

a <- ggplot(pMSE_ex4_nCW, aes(x = term_numeric, y = pMSE, fill = term_numeric)) +
  geom_violin(alpha = 0.1, fill = "white", color = "black") +
  geom_boxplot(alpha = 0.2, width = 0.15, fill = "blue", color = "black") +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=1, binwidth=0.000005, alpha = 0.3, color = "blue") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank()) +  
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  ggtitle("Global utility: pMSE") +
  xlab("") + ylab("pMSE")
a

b <- ggplot(PO50_ex4_nCW, aes(x = term_numeric, y = PO50, fill = term_numeric)) +
  geom_violin(alpha = 0.1, fill = "white", color = "black") +
  geom_boxplot(alpha = 0.2, width = 0.15, fill = "blue", color = "black") +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=2, binwidth=0.01, alpha = 0.3, color = "blue") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank()) +  
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  ggtitle("Global utility: PO50") +
  xlab("") + ylab("PO50")
b

#### Computational Time

time <- ggplot(time.elapsed_ex4_nCW, aes(x = term_numeric, y = Time, fill = term_numeric)) +
  geom_violin(alpha = 0.1, fill = "white", color = "black") +
  geom_boxplot(alpha = 0.2, width = 0.15, fill = "blue", color = "black") +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=4, binwidth=0.0005, alpha = 0.3, color = "blue") +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank()) +  
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  ggtitle("Computational performance: Time (s)") +
  xlab("") + ylab("Time (s)")

time

########################################

layout <- "
AABBCC
AABBCC
AABBCC

"

a + b + time + plot_layout(design = layout) + plot_annotation(tag_levels = 'i')

###############################################################################


#### SPECIFIC UTILITY PERFORMANCE

# Define a vector with the desired order of the categories
new_order <- c("conf.high", "estimate", "conf.low")

# Create a new factor variable with the desired order
results_df_long_ex4_nCW$variable_order <- factor(results_df_long_ex4_nCW$variable, levels = new_order)
results_df_long_ex4_nCW_s$variable_order <- factor(results_df_long_ex4_nCW_s$variable, levels = new_order)

c1 <- ggplot(results_df_ex4_nCW, aes(x = term_numeric, y = p.value)) +
  # geom_violin(alpha = 0.1) +
  geom_dotplot(aes(fill = ifelse(p.value < 0.05, "red", "blue")),
               binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=3, binwidth=0.005, alpha = 0.5) +
  geom_point(data = subset(final, term == "ACWR4wks"), aes(x = term_numeric, y = p.value), size = 4) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank()) +  
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  scale_y_continuous(limits = c(0, 1.1)) + # Add this line to set the y-limits to be between 0 and 1
  ggtitle(expression("GEE"~italic(p)~"values")) +
  xlab("ACWR 4 weeks") + ylab(expression(~italic(p)~"value")) +
  annotate("text", x = 1, y = 1.1, label = paste(round(p_below_05_ex4_nCW, 2), "% of the simulations", 
                                                 "\n were below p = 0.05"), size = 4)

c1 

d1 <- ggplot(results_df_long_ex4_nCW, aes(x = variable_order, y = value)) +
  # geom_violin(alpha = 0.1) +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=2.5, binwidth=0.02, alpha = 0.3, fill = "#d3d3d3") +
  geom_boxplot(alpha = 0.5, width = 0.15) +
  geom_point(data = subset(final_long, term == "ACWR4wks"), aes(x = variable, y = value), size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(-2, 4.25)) + 
  ggtitle("GEE estimates and CIs") +
  xlab("ACWR 4 weeks") + ylab("GEE Coefficients")

d1

c2 <- ggplot(results_df_ex4_nCW_s, aes(x = term_numeric, y = p.value)) +
  # geom_violin(alpha = 0.1) +
  geom_dotplot(aes(fill = ifelse(p.value < 0.05, "red", "blue")),
               binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=3, binwidth=0.005, alpha = 0.5) +
  geom_point(data = subset(final_minus, term == "ACWR4wks"), aes(x = term_numeric, y = p.value), size = 4) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank()) +  
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  scale_y_continuous(limits = c(0, 1.1)) + # Add this line to set the y-limits to be between 0 and 1
  ggtitle(expression("GEE"~italic(p)~"values")) +
  xlab("ACWR 4 weeks") + ylab(expression(~italic(p)~"value")) +
  annotate("text", x = 1, y = 1.1, label = paste(round(p_below_05_ex4_nCW_s, 2), "% of the simulations", 
                                                 "\n were below p = 0.05"), size = 4)

c2 

d2 <- ggplot(results_df_long_ex4_nCW_s, aes(x = variable_order, y = value)) +
  # geom_violin(alpha = 0.1) +
  geom_dotplot(binaxis='y', stackdir='center',stackratio=1.2, 
               dotsize=2.5, binwidth=0.02, alpha = 0.3, fill = "#d3d3d3") +
  geom_boxplot(alpha = 0.5, width = 0.15) +
  geom_point(data = subset(final_long_minus, term == "ACWR4wks"), aes(x = variable, y = value), size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  scale_y_continuous(limits = c(-2, 4.25)) + 
  ggtitle("GEE estimates and CIs") +
  xlab("ACWR 4 weeks") + ylab("GEE Coefficients")

d2

####

#####################################

#### Acute Workload

means1 <- colMeans(player1_acute_load_ex4_nCW, na.rm = TRUE)
lower1 <- apply(player1_acute_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper1 <- apply(player1_acute_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player1_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 1),
  Mean = means1,
  Lower = lower1,
  Upper = upper1
)

# Plot the mean and confidence interval for participant 1
e <- ggplot(player1_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 1), aes(x = WeekID, y = AcuteLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 1 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Acute Load") +
  ggtitle("Participant 1")

e

####

means4 <- colMeans(player4_acute_load_ex4_nCW, na.rm = TRUE)
lower4 <- apply(player4_acute_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper4 <- apply(player4_acute_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player4_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 4),
  Mean = means4,
  Lower = lower4,
  Upper = upper4
)

# Plot the mean and confidence interval for participant 4
f <- ggplot(player4_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 4), aes(x = WeekID, y = AcuteLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 4 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Acute Load") +
  ggtitle("Participant 4")

f

####

means7 <- colMeans(player7_acute_load_ex4_nCW, na.rm = TRUE)
lower7 <- apply(player7_acute_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper7 <- apply(player7_acute_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player7_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 7),
  Mean = means7,
  Lower = lower7,
  Upper = upper7
)

# Plot the mean and confidence interval for participant 7
g <- ggplot(player7_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 7), aes(x = WeekID, y = AcuteLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 7 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Acute Load") +
  ggtitle("Participant 7")

g

####

means11 <- colMeans(player11_acute_load_ex4_nCW, na.rm = TRUE)
lower11 <- apply(player11_acute_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper11 <- apply(player11_acute_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player11_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 11),
  Mean = means11,
  Lower = lower11,
  Upper = upper11
)

# Plot the mean and confidence interval for participant 11
h <- ggplot(player11_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 11), aes(x = WeekID, y = AcuteLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 11 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Acute Load") +
  ggtitle("Participant 11")

h

####

means15 <- colMeans(player15_acute_load_ex4_nCW, na.rm = TRUE)
lower15 <- apply(player15_acute_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper15 <- apply(player15_acute_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player15_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 15),
  Mean = means15,
  Lower = lower15,
  Upper = upper15
)

# Plot the mean and confidence interval for participant 11
i <- ggplot(player15_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 15), aes(x = WeekID, y = AcuteLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 15 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Acute Load") +
  ggtitle("Participant 15")

i

#### Chronic Workload

means1 <- colMeans(player1_chronic_load_ex4_nCW, na.rm = TRUE)
lower1 <- apply(player1_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper1 <- apply(player1_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player1_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 1),
  Mean = means1,
  Lower = lower1,
  Upper = upper1
)

# Plot the mean and confidence interval for participant 1
j <- ggplot(player1_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 1), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 1 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 1")

j

####

means4 <- colMeans(player4_chronic_load_ex4_nCW, na.rm = TRUE)
lower4 <- apply(player4_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper4 <- apply(player4_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player4_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 4),
  Mean = means4,
  Lower = lower4,
  Upper = upper4
)

# Plot the mean and confidence interval for participant 4
k <- ggplot(player4_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 4), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 4 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 4")

k

####

means7 <- colMeans(player7_chronic_load_ex4_nCW, na.rm = TRUE)
lower7 <- apply(player7_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper7 <- apply(player7_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player7_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 7),
  Mean = means7,
  Lower = lower7,
  Upper = upper7
)

# Plot the mean and confidence interval for participant 7
l <- ggplot(player7_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 7), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 7 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 7")

l

####

means11 <- colMeans(player11_chronic_load_ex4_nCW, na.rm = TRUE)
lower11 <- apply(player11_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper11 <- apply(player11_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player11_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 11),
  Mean = means11,
  Lower = lower11,
  Upper = upper11
)

# Plot the mean and confidence interval for participant 11
m <- ggplot(player11_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 11), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 11 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 11")

m

####

means15 <- colMeans(player15_chronic_load_ex4_nCW, na.rm = TRUE)
lower15 <- apply(player15_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper15 <- apply(player15_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player15_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 15),
  Mean = means15,
  Lower = lower15,
  Upper = upper15
)

# Plot the mean and confidence interval for participant 11
n <- ggplot(player15_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 15), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 15 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 15")

n


#### Chronic Workload (Caluclated)

means1 <- colMeans(player1_s_chronic_load_ex4_nCW, na.rm = TRUE)
lower1 <- apply(player1_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper1 <- apply(player1_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player1_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 1),
  Mean = means1,
  Lower = lower1,
  Upper = upper1
)

# Plot the mean and confidence interval for participant 1
o <- ggplot(player1_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 1), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 1 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 1")

o

####

means4 <- colMeans(player4_s_chronic_load_ex4_nCW, na.rm = TRUE)
lower4 <- apply(player4_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper4 <- apply(player4_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player4_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 4),
  Mean = means4,
  Lower = lower4,
  Upper = upper4
)

# Plot the mean and confidence interval for participant 4
p <- ggplot(player4_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 4), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 4 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 4")

p

####

means7 <- colMeans(player7_s_chronic_load_ex4_nCW, na.rm = TRUE)
lower7 <- apply(player7_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper7 <- apply(player7_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player7_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 7),
  Mean = means7,
  Lower = lower7,
  Upper = upper7
)

# Plot the mean and confidence interval for participant 7
q <- ggplot(player7_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 7), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 7 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 7")

q

####

means11 <- colMeans(player11_s_chronic_load_ex4_nCW, na.rm = TRUE)
lower11 <- apply(player11_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper11 <- apply(player11_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player11_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 11),
  Mean = means11,
  Lower = lower11,
  Upper = upper11
)

# Plot the mean and confidence interval for participant 11
r <- ggplot(player11_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 11), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 11 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 11")

r

####

means15 <- colMeans(player15_s_chronic_load_ex4_nCW, na.rm = TRUE)
lower15 <- apply(player15_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.025, na.rm = TRUE)
upper15 <- apply(player15_s_chronic_load_ex4_nCW, 2, quantile, probs = 0.975, na.rm = TRUE)

# Combine the means and bounds into a data frame
player15_summary <- data.frame(
  WeekID = 1:sum(df$PlayerID == 15),
  Mean = means15,
  Lower = lower15,
  Upper = upper15
)

# Plot the mean and confidence interval for participant 11
s <- ggplot(player15_summary, aes(x = WeekID, y = Mean)) +
  geom_line(alpha = 0.3) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = subset(df, PlayerID == 15), aes(x = WeekID, y = ChronicLoad), color = "blue") +
  geom_vline(data = subset(df, PlayerID == 15 & Injury == 1), aes(xintercept = WeekID), color = "red", size = 1) +
  theme_classic() +
  xlab("Week ID") +
  ylab("Chronic Load") +
  ggtitle("Participant 15")

s

################################################################################

tiff("Figure_2.tiff", units="in", width=13, height=15.5, res=600)

right_plot <- wrap_elements((c1 + d1) + 
                              plot_annotation(title = "A. Chronic Load Scenario 1: treated as independent variable",
                                              theme = theme(plot.title = element_text(size = 14, face = "bold"))))

left_plot <- wrap_elements((c2 + d2) + 
                             plot_annotation(title = "B. Chronic Load Scenario 2: calculated from synthetic acute load",
                                             theme = theme(plot.title = element_text(size = 14, face = "bold"))))

third_plot <- wrap_elements((e | g | h | i) + 
                              plot_annotation(title = "C. Acute Load",
                                              theme = theme(plot.title = element_text(size = 14, face = "bold"))))

fourth_plot <- wrap_elements((j | l | m | n) + 
                               plot_annotation(title = "D. Chronic Load Scenario 1: treated as independent variable",
                                               theme = theme(plot.title = element_text(size = 14, face = "bold"))))

fifth_plot <- wrap_elements((o | q | r | s) + 
                              plot_annotation(title = "E. Chronic Load Scenario 2: calculated from synthetic acute load",
                                              theme = theme(plot.title = element_text(size = 14, face = "bold"))))

# (right_plot + left_plot) / third_plot / fourth_plot / fifth_plot

layout <- "
AAAAAAAABBBBBBBB
AAAAAAAABBBBBBBB
CCCCCCCCCCCCCCCC
DDDDDDDDDDDDDDDD
EEEEEEEEEEEEEEEE

"

right_plot + left_plot + third_plot + fourth_plot + fifth_plot + 
  plot_layout(design = layout) + plot_annotation(title = "Simulation Condition Four: GEE and Individual Player Results", 
                                                 theme = list(
                                                   plot.title = element_text(face = "bold", size = rel(1.5))))

dev.off()

################################################################################

#### Performance numeric results

# Here we continue making some of the metrics to be summarized in a table, which
# will follow. 

# Add std.error residuals. Earlier for the plots we made residuals for the GEE
# estimate, upper and lower bound of CIs. We did not make residuals for the SE
# comparing synthetic data to the original data. That takes place below. 

results_df_ex4_nCW$std.error.resid <- results_df_ex4_nCW$std.error - t(as.data.frame(rep(final[2,3],nrow(results_df_ex4_nCW))))
results_df_ex4_nCW_s$std.error.resid <- results_df_ex4_nCW_s$std.error - t(as.data.frame(rep(final_minus[2,3],nrow(results_df_ex4_nCW_s))))

#### MAE of GEE Outcomes. ######################################################

## Below we turn the residuals into absolute errors. Later in the table we 
# calculate the mean of these to get the MAE for all metrics below. 

conf.low.abs <- abs(data.frame("conf.low.resid" = results_df_ex4_nCW$conf.low.resid)) 
conf.low.abs_s <- abs(data.frame("conf.low.resid_s" = results_df_ex4_nCW_s$conf.low.resid)) 

conf.high.abs <- abs(data.frame("conf.high.resid" = results_df_ex4_nCW$conf.high.resid)) 
conf.high.abs_s <- abs(data.frame("conf.high.resid_s" = results_df_ex4_nCW_s$conf.high.resid))

estimate.abs <- abs(data.frame("estimate.resid" = results_df_ex4_nCW$estimate.resid)) 
estimate.abs_s <- abs(data.frame("estimate.resid_s" = results_df_ex4_nCW_s$estimate.resid)) 

std.error.abs <- abs(data.frame("std.error.resid" = results_df_ex4_nCW$std.error.resid))
std.error.abs_s <- abs(data.frame("std.error.resid_s" = results_df_ex4_nCW_s$std.error.resid)) 

#### CREATING A SUMMARISED TABLE OF RESULTS ####################################

## The following looks at creating a summarized results table, which will be 
# exported. 

# create a list of data frames of metrics to be included.
dfs <- list(pMSE_ex4_nCW, 
            s_pMSE_ex4_nCW, 
            PO50_ex4_nCW, 
            time.elapsed_ex4_nCW, 
            AcuteLoad_RMSE_ex4_nCW, 
            ChronicLoad_RMSE_ex4_nCW, 
            ChronicLoad_RMSE_ex4_nCW_s,
            conf.low.abs,
            conf.low.abs_s,
            conf.high.abs,
            conf.high.abs_s,
            estimate.abs,
            estimate.abs_s,
            std.error.abs,
            std.error.abs_s,
            p_below_05_err_ex4_nCW,
            p_below_05_err_ex4_nCW_s)

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
write_xlsx(table,"C:\\Users\\159479\\OneDrive - UTS\\Meta-Research\\2. Synthetic Data\\Experimental Paper\\1. Completed Article & Code\\Results_SC4.xlsx")

################################################################################
