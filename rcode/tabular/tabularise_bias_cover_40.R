#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Tabularise summaries percentage bias + coverage
## lindanab4@gmail.com - 20201103
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
library(xtable)
library(data.table)
library(dplyr)
sum_analysis <- 
  readRDS(file = "./results/summaries/summary.Rds")
sum_analysis <- subset(
  sum_analysis,
  size_valdata == 0.4 &
    (
      R_squared == 0.2 |
        R_squared == 0.4 | R_squared == 0.6 | R_squared == 0.8
    ) &
    (skewness == 0.1 | skewness == 1.5 | skewness == 3) &
    (method == "complete_case" | method == "inadm_reg_cal")
)
sum_analysis <- sum_analysis[order(sampling_strat, -linear, method, R_squared, skewness),]

##############################
# 1 - Helper functions 
##############################
# # Function that creates a string of effect_est% (ci_lower%-ci_upper%)
# effect_est_and_ci <- function(row_of_summary){
#   effect_est <- round(as.numeric(row_of_summary[["effect_est"]]), 0)
#   ci_lower <- round(as.numeric(row_of_summary[["ci_lower"]]), 0)
#   ci_upper <- round(as.numeric(row_of_summary[["ci_upper"]]), 0)
#   paste0(effect_est, "%", " (", ci_lower, "%-", ci_upper, "%)")
# }

##############################
# 2 - Create table
##############################
# Select values needed from summary object
caption <- 
  c("Percentage bias and coverage in the simulation study")
table <-
  sum_analysis[, c("sampling_strat",
                   "linear", 
                   "method",
                   "R_squared",
                   "skewness",
                   "perc_bias",
                   "cover")]
table$perc_bias <- round(table$perc_bias, 1)
table$cover <- round(table$cover * 100, 1)
table_ivrs <- subset(table, method == "complete_case")
table_ivrs <- table_ivrs[, -c("method")]
colnames(table_ivrs)[colnames(table_ivrs) == "perc_bias"] <- "perc_bias_ivrs"
colnames(table_ivrs)[colnames(table_ivrs) == "cover"] <- "cover_ivrs"
table_ivrs_wide <- reshape(
  table_ivrs,
  idvar = c("linear", "R_squared", "skewness"),
  timevar = "sampling_strat",
  direction = "wide"
)
table_ivrs_wide <- setcolorder(table_ivrs_wide,
                               c(1, 2, 3, 
                                 4, 6, 8,
                                 5, 7, 9))


table_vrc <- subset(table, method == "inadm_reg_cal")
table_vrc <- table_vrc[, -c("method")]
colnames(table_vrc)[colnames(table_vrc) == "perc_bias"] <- "perc_bias_vrc"
colnames(table_vrc)[colnames(table_vrc) == "cover"] <- "cover_vrc"
table_vrc_wide <- reshape(
  table_vrc,
  idvar = c("linear", "R_squared", "skewness"),
  timevar = "sampling_strat",
  direction = "wide"
)
table_vrc_wide <- setcolorder(table_vrc_wide,
                               c(1, 2, 3, 
                                 4, 6, 8,
                                 5, 7, 9))

table <- merge(table_ivrs_wide, table_vrc_wide, 
               by = c("linear", "R_squared", "skewness"), sort = F)
table$linear <- ifelse(table$linear == 0, "no", "yes")
# Change columnames 
table <- as.data.frame(table)
colnames(table) <- c("", "Scenario", "", 
                     "", "", "IVSR", "", "", "",
                     "", "", "VRC", "", "", "")
table <- rbind(c("", "", "",
                 "R", "SR", "E", 
                 "R", "SR", "E",
                 "R", "SR", "E",
                 "R", "SR", "E"), table)
table <- rbind(c("Linear", "R-Squared", "Skewness",
                 "", "Bias", "", 
                 "", "Coverage", "",
                 "", "Bias", "",
                 "", "Coverage", ""), table)

# Create TeX table
table_xtable <- print(xtable(table, 
                             caption = caption,
                             digits = 0), 
                      include.rownames = FALSE)
file_con <- file("./results/tables/table_bias_cover_40.txt")
writeLines(table_xtable, file_con)
close(file_con)
