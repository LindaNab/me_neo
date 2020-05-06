#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Tabularise summaries differential measurement error
## lindanab4@gmail.com - 20200425
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
library(xtable)
sum_analysis_diffme <- 
  readRDS(file = "./results/summaries/summary_diffme.Rds")

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
  c("Percentage bias in the estimated conditional effect of VAT on IR in case of differential measurement error")
table <-
  sum_analysis_diffme[, c("method",
                          "sampling_strat",
                          "perc_bias")]
table$perc_bias <- round(table$perc_bias, 0)
# # Format estimates from analysis
# table2 <- cbind(table2[, c("method", "sampling_strat")],
#                 apply(table2,
#                       1,
#                       FUN = effect_est_and_ci))
# Format table2 to long format
table <- reshape(table,
                 idvar = "method",
                 timevar = "sampling_strat",
                 direction = "wide")
# Change columnames 
colnames(table) <- c("Method", "Random", "Uniform", "Extremes")
# Change rownames
table$Method <- c(
  "Complete case",
  "Naive",
  "Regression calibration",
  "Efficient regression calibration",
  "Inadmissible regression calibration"
)
# Create TeX table
table_xtable <- print(xtable(table, 
                             caption = caption,
                             digits = 0), 
                      include.rownames = FALSE)
file_con <- file("./results/tables/table_diffme.txt")
writeLines(table_xtable, file_con)
close(file_con)