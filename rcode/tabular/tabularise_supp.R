#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
## Simulation study
##
## Tabularise summaries percentage bias + coverage
## for online supplement results EFFICIENT REG CAL & REG CAL
## lindanab4@gmail.com - 20201117
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
    (size_valdata == 0.1 | size_valdata == 0.25 | size_valdata == 0.4) &
    (
      R_squared == 0.2 |
        R_squared == 0.4 | R_squared == 0.6 | R_squared == 0.8 | 
        R_squared == 0.9
    ) &
    (skewness == 0.1 | skewness == 1.5 | skewness == 3 | skewness == 1.0) &
    (method == "efficient_reg_cal" | method == "reg_cal")
)
sum_analysis <- sum_analysis[order(sampling_strat, 
                                   method, 
                                   -linear, 
                                   skewness, 
                                   R_squared),]

##############################
# 1 - Helper functions 
##############################
# makes wide format of a particular sample size (ss) of valdata
subset_table_size_valdata <- function(table_method, 
                                      use_size_valdata,
                                      mse = F){
  table_method_ss <- subset(table_method, size_valdata == use_size_valdata)
  table_method_ss <- table_method_ss[,-c("size_valdata")]
  if (mse == T){
    colnames(table_method_ss)[colnames(table_method_ss) == "mse"] <-
      paste0("mse_", use_size_valdata)
  }
  if (mse == F){
    colnames(table_method_ss)[colnames(table_method_ss) == "perc_bias"] <-
      paste0("perc_bias_", use_size_valdata)
    colnames(table_method_ss)[colnames(table_method_ss) == "cover"] <-
      paste0("cover_", use_size_valdata)
  }

  table_method_ss_wide <- reshape(
    table_method_ss,
    idvar = c("linear", "R_squared", "skewness"),
    timevar = "sampling_strat",
    direction = "wide"
  )
  if (mse == F){
    table_method_ss_wide <- setcolorder(table_method_ss_wide,
                                        c(1, 2, 3,
                                          4, 6, 8,
                                          5, 7, 9))
  }
  # if (mse == T) {
  #   table_method_ss_wide <- setcolorder(table_method_ss_wide,
  #                                       c(1, 2, 3,
  #                                         4, 7, 10,
  #                                         5, 8, 11,
  #                                         6, 9, 12))
  # }
  table_method_ss_wide
}

##############################
# 2 - Create table
##############################
# Select values needed from summary object
caption <-  
  c("Percentage bias and coverage in the estimated association between visceral adipose tissue and insulin resistance by application of efficient regression calibration")
table <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "perc_bias",
                   "cover")]
table$perc_bias <- round(table$perc_bias, 1)
table$cover <- round(table$cover * 100, 1)
table$linear <- ifelse(table$linear == 0, "no", "yes")

####### EFFICIENT REGCAL #######
table_erc <- subset(table, method == "efficient_reg_cal")
table_erc <- table_erc[, -c("method")]
table_erc_10_wide <- subset_table_size_valdata(table_erc,
                                               0.10)
table_erc_25_wide <- subset_table_size_valdata(table_erc,
                                               0.25)
table_erc_40_wide <- subset_table_size_valdata(table_erc,
                                               0.40)
# make one table of all three sizes of valdata
table_erc_wide <- 
left_join(table_erc_40_wide,
          left_join(table_erc_25_wide,
                    table_erc_10_wide,
                    by = c("linear", "skewness", "R_squared")),
          by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_erc_wide <- as.data.frame(table_erc_wide)
colnames(table_erc_wide) <- c("", "Scenario", "", 
                              "", "", "40%", "", "", "",
                              "", "", "25%", "", "", "",
                              "", "", "10%", "", "", "")
table_erc_wide <- rbind(c("", "", "",
                 "R", "SR", "E", 
                 "R", "SR", "E",
                 "R", "SR", "E",
                 "R", "SR", "E",
                 "R", "SR", "E",
                 "R", "SR", "E"), table_erc_wide)
table_erc_wide <- rbind(c("Linear", "Skewness", "R-Squared", 
                 "", "Bias", "", 
                 "", "Coverage", "",
                 "", "Bias", "",
                 "", "Coverage", "",
                 "", "Bias", "",
                 "", "Coverage", ""), table_erc_wide)
# Create TeX table
table_erc_wide_xtable <- print(xtable(table_erc_wide,
                                      caption = caption,
                                      digits = 0),
                               include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_erc.txt")
writeLines(table_erc_wide_xtable, file_con)
close(file_con)

####### STANDARD REGCAL #######
# Select values needed from summary object
caption <-  
  c("Percentage bias and coverage in the estimated association between visceral adipose tissue and insulin resistance by application of standard regression calibration")
table2 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   #"mse",
                   "perc_bias",
                   "cover")]
#table2$mse <- round(table2$mse, 3)
table2$perc_bias <- round(table2$perc_bias, 1)
table2$cover <- round(table2$cover * 100, 1)
table2$linear <- ifelse(table2$linear == 0, "no", "yes")

table_rc <- subset(table2, method == "reg_cal")
table_rc <- table_rc[, -c("method")]
table_rc_10_wide <- subset_table_size_valdata(table_rc,
                                              0.10,
                                              mse = F)
table_rc_25_wide <- subset_table_size_valdata(table_rc,
                                              0.25,
                                              mse = F)
table_rc_40_wide <- subset_table_size_valdata(table_rc,
                                              0.40,
                                              mse = F)
# make one table of all three sizes of valdata
table_rc_wide <- 
  left_join(table_rc_40_wide,
            left_join(table_rc_25_wide,
                      table_rc_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_rc_wide <- as.data.frame(table_rc_wide)
colnames(table_rc_wide) <- c("", "Scenario", "", 
                             #"", "", 
                             "", "", "40%", "", "", "", 
                             #"",
                             #"", "", 
                             "", "", "25%", "", "", "", 
                             #"",
                             #"", "", 
                             "", "", "10%", "", "", ""
                             #, 
                             #""
                             )
table_rc_wide <- rbind(c("", "", "",
                         #"R", "SR", "E", 
                         #"R", "SR", "E",
                         #"R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E", 
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E"), table_rc_wide)
table_rc_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                         #"", "MSE", "",
                         "", "Bias", "", 
                         "", "Coverage", "",
                         #"", "MSE", "",
                         "", "Bias", "",
                         "", "Coverage", "",
                         #"", "MSE", "",
                         "", "Bias", "",
                         "", "Coverage", ""), table_rc_wide)
# Create TeX table
table_rc_wide_xtable <- print(xtable(table_rc_wide,
                                      caption = caption,
                                      digits = 0),
                               include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_rc.txt")
writeLines(table_rc_wide_xtable, file_con)
close(file_con)

####### STANDARD REGCAL MSE #######
# Select values needed from summary object
caption <-  
  c("Mean squared error of the estimated association between visceral adipose tissue and insulin resistance by application of standard regression calibration")
table3 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "mse")]
table3$mse <- round(table3$mse, 3)
table3$linear <- ifelse(table3$linear == 0, "no", "yes")

table_rc_mse <- subset(table3, method == "reg_cal")
table_rc_mse <- table_rc_mse[, -c("method")]
table_rc_mse_10_wide <- subset_table_size_valdata(table_rc_mse,
                                              0.10,
                                              mse = T)
table_rc_mse_25_wide <- subset_table_size_valdata(table_rc_mse,
                                              0.25,
                                              mse = T)
table_rc_mse_40_wide <- subset_table_size_valdata(table_rc_mse,
                                              0.40,
                                              mse = T)
# make one table of all three sizes of valdata
table_rc_mse_wide <- 
  left_join(table_rc_mse_40_wide,
            left_join(table_rc_mse_25_wide,
                      table_rc_mse_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_rc_mse_wide <- as.data.frame(table_rc_mse_wide)
colnames(table_rc_mse_wide) <- c("", "Scenario", "", 
                             "", "40%", "",
                             "", "25%", "",
                             "", "10%", ""
)
table_rc_mse_wide <- rbind(c("", "", "",
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E"), table_rc_mse_wide)
table_rc_mse_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                         "", "MSE", "",
                         "", "MSE", "",
                         "", "MSE", ""), table_rc_mse_wide)
# Create TeX table
table_rc_mse_wide_xtable <- print(xtable(table_rc_mse_wide,
                                     caption = caption,
                                     digits = 0),
                              include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_rc_mse.txt")
writeLines(table_rc_mse_wide_xtable, file_con)
close(file_con)
