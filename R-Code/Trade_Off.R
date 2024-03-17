
#### Trade-Offs

library(ggplot2)
library(tidyr)
library(dplyr)

################################################################################

# ONLY PLAY THIS CODE ONCE YOU HAVE PLAYED SIMULATIONS 1-4. 

################################################################################

#### Data Manipulation

results_df_ex1_nCW <- cbind(results_df_ex1_nCW, p_below_05_err_ex1_nCW)
results_df_ex1_nCW_s <- cbind(results_df_ex1_nCW_s, p_below_05_err_ex1_nCW_s)

p_df_long_ex1_nCW <- gather(results_df_ex1_nCW, key = "variable", value = "value", p_below_05_err_ex1_nCW)
p_df_long_ex1_nCW_s <- gather(results_df_ex1_nCW_s, key = "variable", value = "value", p_below_05_err_ex1_nCW_s)

p_df_long_ex1_nCW <- p_df_long_ex1_nCW %>%
  mutate(Trial = "Experiment 1 (S1)")
p_df_long_ex1_nCW_s <- p_df_long_ex1_nCW_s %>%
  mutate(Trial = "Experiment 1 (S2)")

####

results_df_ex2_nCW <- cbind(results_df_ex2_nCW, p_below_05_err_ex2_nCW)
results_df_ex2_nCW_s <- cbind(results_df_ex2_nCW_s, p_below_05_err_ex2_nCW_s)

p_df_long_ex2_nCW <- gather(results_df_ex2_nCW, key = "variable", value = "value", p_below_05_err_ex2_nCW)
p_df_long_ex2_nCW_s <- gather(results_df_ex2_nCW_s, key = "variable", value = "value", p_below_05_err_ex2_nCW_s)

p_df_long_ex2_nCW <- p_df_long_ex2_nCW %>%
  mutate(Trial = "Experiment 2 (S1)")
p_df_long_ex2_nCW_s <- p_df_long_ex2_nCW_s %>%
  mutate(Trial = "Experiment 2 (S2)")

####

results_df_ex3_nCW <- cbind(results_df_ex3_nCW, p_below_05_err_ex3_nCW)
results_df_ex3_nCW_s <- cbind(results_df_ex3_nCW_s, p_below_05_err_ex3_nCW_s)

p_df_long_ex3_nCW <- gather(results_df_ex3_nCW, key = "variable", value = "value", p_below_05_err_ex3_nCW)
p_df_long_ex3_nCW_s <- gather(results_df_ex3_nCW_s, key = "variable", value = "value", p_below_05_err_ex3_nCW_s)

p_df_long_ex3_nCW <- p_df_long_ex3_nCW %>%
  mutate(Trial = "Experiment 3 (S1)")
p_df_long_ex3_nCW_s <- p_df_long_ex3_nCW_s %>%
  mutate(Trial = "Experiment 3 (S2)")

####

results_df_ex4_nCW <- cbind(results_df_ex4_nCW, p_below_05_err_ex4_nCW)
results_df_ex4_nCW_s <- cbind(results_df_ex4_nCW_s, p_below_05_err_ex4_nCW_s)

p_df_long_ex4_nCW <- gather(results_df_ex4_nCW, key = "variable", value = "value", p_below_05_err_ex4_nCW)
p_df_long_ex4_nCW_s <- gather(results_df_ex4_nCW_s, key = "variable", value = "value", p_below_05_err_ex4_nCW_s)

p_df_long_ex4_nCW <- p_df_long_ex4_nCW %>%
  mutate(Trial = "Experiment 4 (S1)")
p_df_long_ex4_nCW_s <- p_df_long_ex4_nCW_s %>%
  mutate(Trial = "Experiment 4 (S2)")

####

selected_cols <- c("term", "variable", "value", "Trial")  # List of columns you want to include

p_df_long <- rbind(p_df_long_ex1_nCW[, selected_cols], p_df_long_ex1_nCW_s[, selected_cols],
                   p_df_long_ex2_nCW[, selected_cols], p_df_long_ex2_nCW_s[, selected_cols],
                   p_df_long_ex3_nCW[, selected_cols], p_df_long_ex3_nCW_s[, selected_cols],
                   p_df_long_ex4_nCW[, selected_cols], p_df_long_ex4_nCW_s[, selected_cols])

################################################################################

#### Data Manipulation

results_df_ex1_nCW <- cbind(results_df_ex1_nCW, AcuteLoad_RMSE_ex1_nCW)

AcuteLoad_long_ex1_nCW <- gather(results_df_ex1_nCW, key = "variable", value = "value", RMSE)

AcuteLoad_long_ex1_nCW <- AcuteLoad_long_ex1_nCW %>%
  mutate(Trial = "Experiment 1 (S1)")

####

results_df_ex2_nCW <- cbind(results_df_ex2_nCW, AcuteLoad_RMSE_ex2_nCW)

AcuteLoad_long_ex2_nCW <- gather(results_df_ex2_nCW, key = "variable", value = "value", RMSE)

AcuteLoad_long_ex2_nCW <- AcuteLoad_long_ex2_nCW %>%
  mutate(Trial = "Experiment 2 (S1)")

####

results_df_ex3_nCW <- cbind(results_df_ex3_nCW, AcuteLoad_RMSE_ex3_nCW)

AcuteLoad_long_ex3_nCW <- gather(results_df_ex3_nCW, key = "variable", value = "value", RMSE)

AcuteLoad_long_ex3_nCW <- AcuteLoad_long_ex3_nCW %>%
  mutate(Trial = "Experiment 3 (S1)")

####

results_df_ex4_nCW <- cbind(results_df_ex4_nCW, AcuteLoad_RMSE_ex4_nCW)

AcuteLoad_long_ex4_nCW <- gather(results_df_ex4_nCW, key = "variable", value = "value", RMSE)

AcuteLoad_long_ex4_nCW <- AcuteLoad_long_ex4_nCW %>%
  mutate(Trial = "Experiment 4 (S1)")

####

AcuteLoad_long <- rbind(AcuteLoad_long_ex1_nCW, 
                        AcuteLoad_long_ex2_nCW, 
                        AcuteLoad_long_ex3_nCW, 
                        AcuteLoad_long_ex4_nCW)

selected_cols <- c("term", "variable", "value", "Trial")  # List of columns you want to include

AcuteLoad_long <- rbind(AcuteLoad_long_ex1_nCW[, selected_cols], 
                        AcuteLoad_long_ex2_nCW[, selected_cols], 
                        AcuteLoad_long_ex3_nCW[, selected_cols], 
                        AcuteLoad_long_ex4_nCW[, selected_cols])

###############################################################################

#### Data Manipulation

results_df_ex1_nCW <- cbind(results_df_ex1_nCW, ChronicLoad_RMSE_ex1_nCW)
col_names <- names(results_df_ex1_nCW)
col_names[length(col_names)] <- "RMSE.C1"
names(results_df_ex1_nCW) <- col_names

results_df_ex1_nCW_s <- cbind(results_df_ex1_nCW_s, ChronicLoad_RMSE_ex1_nCW_s)
col_names <- names(results_df_ex1_nCW_s)
col_names[length(col_names)] <- "RMSE.C2"
names(results_df_ex1_nCW_s) <- col_names

ChronicLoad_ex1_nCW <- gather(results_df_ex1_nCW, key = "variable", value = "value", RMSE.C1)
ChronicLoad_ex1_nCW_s <- gather(results_df_ex1_nCW_s, key = "variable", value = "value", RMSE.C2)

ChronicLoad_ex1_nCW <- ChronicLoad_ex1_nCW %>%
  mutate(Trial = "Experiment 1 (S1)")
ChronicLoad_ex1_nCW_s <- ChronicLoad_ex1_nCW_s %>%
  mutate(Trial = "Experiment 1 (S2)")

####

results_df_ex2_nCW <- cbind(results_df_ex2_nCW, ChronicLoad_RMSE_ex2_nCW)
col_names <- names(results_df_ex2_nCW)
col_names[length(col_names)] <- "RMSE.C1"
names(results_df_ex2_nCW) <- col_names

results_df_ex2_nCW_s <- cbind(results_df_ex2_nCW_s, ChronicLoad_RMSE_ex2_nCW_s)
col_names <- names(results_df_ex2_nCW_s)
col_names[length(col_names)] <- "RMSE.C2"
names(results_df_ex2_nCW_s) <- col_names

ChronicLoad_ex2_nCW <- gather(results_df_ex2_nCW, key = "variable", value = "value", RMSE.C1)
ChronicLoad_ex2_nCW_s <- gather(results_df_ex2_nCW_s, key = "variable", value = "value", RMSE.C2)

ChronicLoad_ex2_nCW <- ChronicLoad_ex2_nCW %>%
  mutate(Trial = "Experiment 2 (S1)")
ChronicLoad_ex2_nCW_s <- ChronicLoad_ex2_nCW_s %>%
  mutate(Trial = "Experiment 2 (S2)")

####

results_df_ex3_nCW <- cbind(results_df_ex3_nCW, ChronicLoad_RMSE_ex3_nCW)
col_names <- names(results_df_ex3_nCW)
col_names[length(col_names)] <- "RMSE.C1"
names(results_df_ex3_nCW) <- col_names

results_df_ex3_nCW_s <- cbind(results_df_ex3_nCW_s, ChronicLoad_RMSE_ex3_nCW_s)
col_names <- names(results_df_ex3_nCW_s)
col_names[length(col_names)] <- "RMSE.C2"
names(results_df_ex3_nCW_s) <- col_names

ChronicLoad_ex3_nCW <- gather(results_df_ex3_nCW, key = "variable", value = "value", RMSE.C1)
ChronicLoad_ex3_nCW_s <- gather(results_df_ex3_nCW_s, key = "variable", value = "value", RMSE.C2)

ChronicLoad_ex3_nCW <- ChronicLoad_ex3_nCW %>%
  mutate(Trial = "Experiment 3 (S1)")
ChronicLoad_ex3_nCW_s <- ChronicLoad_ex3_nCW_s %>%
  mutate(Trial = "Experiment 3 (S2)")

####

results_df_ex4_nCW <- cbind(results_df_ex4_nCW, ChronicLoad_RMSE_ex4_nCW)
col_names <- names(results_df_ex4_nCW)
col_names[length(col_names)] <- "RMSE.C1"
names(results_df_ex4_nCW) <- col_names

results_df_ex4_nCW_s <- cbind(results_df_ex4_nCW_s, ChronicLoad_RMSE_ex4_nCW_s)
col_names <- names(results_df_ex4_nCW_s)
col_names[length(col_names)] <- "RMSE.C2"
names(results_df_ex4_nCW_s) <- col_names

ChronicLoad_ex4_nCW <- gather(results_df_ex4_nCW, key = "variable", value = "value", RMSE.C1)
ChronicLoad_ex4_nCW_s <- gather(results_df_ex4_nCW_s, key = "variable", value = "value", RMSE.C2)

ChronicLoad_ex4_nCW <- ChronicLoad_ex4_nCW %>%
  mutate(Trial = "Experiment 4 (S1)")
ChronicLoad_ex4_nCW_s <- ChronicLoad_ex4_nCW_s %>%
  mutate(Trial = "Experiment 4 (S2)")

####

selected_cols <- c("term", "variable", "value", "Trial")  # List of columns you want to include

ChronicLoad_long <- rbind(ChronicLoad_ex1_nCW[, selected_cols], ChronicLoad_ex1_nCW_s[, selected_cols],
                     ChronicLoad_ex2_nCW[, selected_cols], ChronicLoad_ex2_nCW_s[, selected_cols],
                     ChronicLoad_ex3_nCW[, selected_cols], ChronicLoad_ex3_nCW_s[, selected_cols],
                     ChronicLoad_ex4_nCW[, selected_cols], ChronicLoad_ex4_nCW_s[, selected_cols])

################################################################################

# oONLY PLAY THE CODE ABOVE ONCE.

################################################################################

# Assuming your data frame is called AcuteLoad_long
p_df_long <- p_df_long %>%
  mutate(Trial = case_when(
    Trial == "Experiment 1 (S2)" ~ "Experiment 1",
    Trial == "Experiment 2 (S2)" ~ "Experiment 2",
    Trial == "Experiment 3 (S2)" ~ "Experiment 3",
    Trial == "Experiment 4 (S2)" ~ "Experiment 4",
    TRUE ~ Trial  # Keep other values unchanged
  ))

# Assuming your data frame is called AcuteLoad_long
p_df_long <- p_df_long %>%
  mutate(Trial = case_when(
    Trial == "Experiment 1" ~ "Simulation Condition 1",
    Trial == "Experiment 2" ~ "Simulation Condition 2",
    Trial == "Experiment 3" ~ "Simulation Condition 3",
    Trial == "Experiment 4" ~ "Simulation Condition 4",
    TRUE ~ Trial  # Keep other values unchanged
  ))

# Assuming ChronicLoad_long is your dataframe after the previous mutation
p_df_long_filt <- p_df_long %>%
  filter(Trial %in% c("Simulation Condition 1", "Simulation Condition 2", "Simulation Condition 3", "Simulation Condition 4"))

x1 <- ggplot(p_df_long_filt, aes(x = Trial, y = value, fill = term)) +
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.2, 
               dotsize=2.5, binwidth=0.01, alpha = 0.3) +
  geom_boxplot(alpha = 0.5, width = 0.15) +
  # geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  scale_y_log10(limits = c(0.001, 1.0)) +  # Using log scale and adjusting limits
  # scale_x_discrete(labels = c("p-value RMSE")) +
  ggtitle("C. MAE of GEE ACWR-4wks p-values (scenario 2: calculated from synthetic acute load)") +
  xlab("") + ylab("GEE p-value MAE")

x1

################################################################################

# Assuming your data frame is called AcuteLoad_long
AcuteLoad_long <- AcuteLoad_long %>%
  mutate(Trial = case_when(
    Trial == "Experiment 1 (S1)" ~ "Experiment 1",
    Trial == "Experiment 2 (S1)" ~ "Experiment 2",
    Trial == "Experiment 3 (S1)" ~ "Experiment 3",
    Trial == "Experiment 4 (S1)" ~ "Experiment 4",
    TRUE ~ Trial  # Keep other values unchanged
  ))

# Assuming your data frame is called AcuteLoad_long
AcuteLoad_long <- AcuteLoad_long %>%
  mutate(Trial = case_when(
    Trial == "Experiment 1" ~ "Simulation Condition 1",
    Trial == "Experiment 2" ~ "Simulation Condition 2",
    Trial == "Experiment 3" ~ "Simulation Condition 3",
    Trial == "Experiment 4" ~ "Simulation Condition 4",
    TRUE ~ Trial  # Keep other values unchanged
  ))

x2 <- ggplot(AcuteLoad_long, aes(x = Trial, y = value, fill = term)) +
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.2, 
               dotsize=4.5, binwidth=0.5, alpha = 0.3) +
  geom_boxplot(alpha = 0.5, width = 0.15) +
  # geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  # scale_y_log10(limits = c(0, 700)) +  # Using log scale and adjusting limits
  # scale_x_discrete(labels = c("p-value RMSE")) +
  ggtitle("A. MAE of Acute Load") +
  xlab("") + ylab("Acute Load MAE")

x2

################################################################################

# Assuming your data frame is called AcuteLoad_long
ChronicLoad_long <- ChronicLoad_long %>%
  mutate(Trial = case_when(
    Trial == "Experiment 1 (S2)" ~ "Experiment 1",
    Trial == "Experiment 2 (S2)" ~ "Experiment 2",
    Trial == "Experiment 3 (S2)" ~ "Experiment 3",
    Trial == "Experiment 4 (S2)" ~ "Experiment 4",
    TRUE ~ Trial  # Keep other values unchanged
  ))

# Assuming your data frame is called AcuteLoad_long
ChronicLoad_long <- ChronicLoad_long %>%
  mutate(Trial = case_when(
    Trial == "Experiment 1" ~ "Simulation Condition 1",
    Trial == "Experiment 2" ~ "Simulation Condition 2",
    Trial == "Experiment 3" ~ "Simulation Condition 3",
    Trial == "Experiment 4" ~ "Simulation Condition 4",
    TRUE ~ Trial  # Keep other values unchanged
  ))

# Assuming ChronicLoad_long is your dataframe after the previous mutation
ChronicLoad_long_filt <- ChronicLoad_long %>%
  filter(Trial %in% c("Simulation Condition 1", "Simulation Condition 2", "Simulation Condition 3", "Simulation Condition 4"))

x3 <- ggplot(ChronicLoad_long_filt, aes(x = Trial, y = value, fill = term)) +
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.2, 
               dotsize=2.5, binwidth=0.5, alpha = 0.3) +
  geom_boxplot(alpha = 0.5, width = 0.15) +
  # geom_hline(yintercept = 0, color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  # scale_y_log10(limits = c(0, 700)) +  # Using log scale and adjusting limits
  # scale_x_discrete(labels = c("p-value RMSE")) +
  ggtitle("B. MAE of Chronic Load (scenario 2: calculated from synthetic acute load)") +
  xlab("") + ylab("Chronic Load MAE")

x3

################################################################################

library(patchwork)

tiff("Figure_3.tiff", units="in", width=9.5, height=12, res=300)

layout <- "
AA
BB
CC

"

x2 + x3 + x1 + plot_layout(design = layout) + plot_annotation(title = "Trade-Offs in Utility", 
                                                              theme = list(
                                                                plot.title = element_text(face = "bold", size = rel(1.5))))

dev.off()

