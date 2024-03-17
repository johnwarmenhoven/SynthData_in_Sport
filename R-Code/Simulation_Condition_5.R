
## Simulation Condition Five ####################################################

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
f
## The following code runs a single trial for simulation condition 5. The regular
# for loop and global and specific utility metrics were not calculated for this
# condition, given that the time taken to generate synthetic data under these 
# specifications took much longer than anticipated. 

start.time <- Sys.time()
synth.obj <- syn(df, 
                  visit.sequence = c(2, 6, 7, 8, 9, 10, 11, 3, 4, 5),
                  method = c("","","cart","cart","cart","","","","","",""),
                  seed = myseed)
synth.obj
end.time <- Sys.time()
time.inj_cart <- end.time - start.time
time.inj_cart

tiff("Simulation_Condition_5.tiff", units="in", width=6, height=12, res=300)

f1<-tableGrob(data.frame("Method" = synth.obj$method))
f2<-tableGrob(data.frame("Visit Sequence" = synth.obj$visit.sequence))
f3<-tableGrob(paste("Time elapsed (s):", (round(time.inj_cart, 2))))

title_grob <- textGrob("Sim_Cond_5", gp = gpar(fontface = "bold"))

grid.arrange(f1,f2,f3,top=title_grob , ncol = 1)

dev.off()

################################################################################



