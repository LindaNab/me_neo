#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Tabularise summaries mse
## lindanab4@gmail.com - 20200425
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
    (sampling_strat == "random" | sampling_strat == "extremes") &
    (method != "naive")
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
  c("Mean squared error of the estimated conditional effect of VAT on IR")
table <-
  sum_analysis[, c("sampling_strat",
                   "linear", 
                   "method",
                   "R_squared",
                   "skewness",
                   "mse")]
table$mse <- round(table$mse, 3)
table_random <- subset(table, sampling_strat == "random")
colnames(table_random)[colnames(table_random) == "mse"] <- "mse_random"
table_random <- table_random %>% select("linear", "method", "R_squared", 
                                        "skewness", "mse_random")
table_random_wide <- reshape(table_random,
                             idvar = c("linear", "R_squared", "skewness"),
                             timevar = "method",
                             direction = "wide")
table_extremes <- subset(table, sampling_strat == "extremes")
colnames(table_extremes)[colnames(table_extremes) == "mse"] <- "mse_extremes"
table_extremes <- table_extremes %>% select("linear", "method", "R_squared", 
                                        "skewness", "mse_extremes")
table_extremes_wide <- reshape(table_extremes,
                             idvar = c("linear", "R_squared", "skewness"),
                             timevar = "method",
                             direction = "wide")
table <- merge(table_random_wide, table_extremes_wide, 
               by = c("linear", "R_squared", "skewness"), sort = F)
table$linear <- ifelse(table$linear == 0, "no", "yes")
# Change columnames 
table <- as.data.frame(table)
colnames(table) <- c("", "Data generation scenario", "", 
                     "", "Random sampling", "", "",
                      "", "Extremes sampling", "", "")
table <- rbind(c("Linear", "R-Squared", "Skewness",
                 "CC", "DRC", "ERC", "IRC",
                 "CC", "DRC", "ERC", "IRC"), table)

# Create TeX table
table_xtable <- print(xtable(table, 
                             caption = caption,
                             digits = 0), 
                      include.rownames = FALSE)
file_con <- file("./results/tables/table_mse.txt")
writeLines(table_xtable, file_con)
close(file_con)
