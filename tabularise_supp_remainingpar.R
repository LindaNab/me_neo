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
sum_analysis <-
  sum_analysis %>%
  filter(R_squared == 0.9 | skewness == 1, 
         method %in% c("complete_case", "inadm_reg_cal", "efficient_reg_cal"))
sum_analysis <- sum_analysis[order(sampling_strat, -linear, method, skewness, R_squared),]

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
caption <-  
  c("Mean squared error of the estimated association between visceral adipose tissue and insulin resistance in the analysis restricted to the internal validation sample")
table4 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "mse")]
table4$mse <- round(table4$mse, 4)
table4$linear <- ifelse(table4$linear == 0, "no", "yes")

table_cc_mse <- subset(table4, method == "complete_case")
table_cc_mse <- table_cc_mse[, -c("method")]
table_cc_mse_10_wide <- subset_table_size_valdata(table_cc_mse,
                                                  0.10,
                                                  mse = T)
table_cc_mse_25_wide <- subset_table_size_valdata(table_cc_mse,
                                                  0.25,
                                                  mse = T)
table_cc_mse_40_wide <- subset_table_size_valdata(table_cc_mse,
                                                  0.40,
                                                  mse = T)
# make one table of all three sizes of valdata
table_cc_mse_wide <- 
  left_join(table_cc_mse_40_wide,
            left_join(table_cc_mse_25_wide,
                      table_cc_mse_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_cc_mse_wide <- as.data.frame(table_cc_mse_wide)
colnames(table_cc_mse_wide) <- c("", "Scenario", "", 
                                 "", "40%", "",
                                 "", "25%", "",
                                 "", "10%", ""
)
table_cc_mse_wide <- rbind(c("", "", "",
                             "R", "SR", "E",
                             "R", "SR", "E",
                             "R", "SR", "E"), table_cc_mse_wide)
table_cc_mse_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                             "", "MSE", "",
                             "", "MSE", "",
                             "", "MSE", ""), table_cc_mse_wide)
# Create TeX table
table_cc_mse_wide_xtable <- print(xtable(table_cc_mse_wide,
                                         caption = caption,
                                         digits = 0),
                                  include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_ivs_mse.txt")
writeLines(table_cc_mse_wide_xtable, file_con)
close(file_con)






caption <-  
  c("Mean squared error of the estimated association between visceral adipose tissue and insulin resistance by application of validation regression calibration")
table5 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "mse")]
table5$mse <- round(table5$mse, 4)
table5$linear <- ifelse(table5$linear == 0, "no", "yes")

table_vrc_mse <- subset(table5, method == "inadm_reg_cal")
table_vrc_mse <- table_vrc_mse[, -c("method")]
table_vrc_mse_10_wide <- subset_table_size_valdata(table_vrc_mse,
                                                  0.10,
                                                  mse = T)
table_vrc_mse_25_wide <- subset_table_size_valdata(table_vrc_mse,
                                                  0.25,
                                                  mse = T)
table_vrc_mse_40_wide <- subset_table_size_valdata(table_vrc_mse,
                                                  0.40,
                                                  mse = T)
# make one table of all three sizes of valdata
table_vrc_mse_wide <- 
  left_join(table_vrc_mse_40_wide,
            left_join(table_vrc_mse_25_wide,
                      table_vrc_mse_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_vrc_mse_wide <- as.data.frame(table_vrc_mse_wide)
colnames(table_vrc_mse_wide) <- c("", "Scenario", "", 
                                 "", "40%", "",
                                 "", "25%", "",
                                 "", "10%", ""
)
table_vrc_mse_wide <- rbind(c("", "", "",
                             "R", "SR", "E",
                             "R", "SR", "E",
                             "R", "SR", "E"), table_vrc_mse_wide)
table_vrc_mse_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                             "", "MSE", "",
                             "", "MSE", "",
                             "", "MSE", ""), table_vrc_mse_wide)
# Create TeX table
table_vrc_mse_wide_xtable <- print(xtable(table_vrc_mse_wide,
                                         caption = caption,
                                         digits = 0),
                                  include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_vrc_mse.txt")
writeLines(table_vrc_mse_wide_xtable, file_con)
close(file_con)









caption <-  
  c("Mean squared error of the estimated association between visceral adipose tissue and insulin resistance by application of efficient regression calibration")
table6 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "mse")]
table6$mse <- round(table6$mse, 4)
table6$linear <- ifelse(table6$linear == 0, "no", "yes")

table_erc_mse <- subset(table6, method == "efficient_reg_cal")
table_erc_mse <- table_erc_mse[, -c("method")]
table_erc_mse_10_wide <- subset_table_size_valdata(table_erc_mse,
                                                   0.10,
                                                   mse = T)
table_erc_mse_25_wide <- subset_table_size_valdata(table_erc_mse,
                                                   0.25,
                                                   mse = T)
table_erc_mse_40_wide <- subset_table_size_valdata(table_erc_mse,
                                                   0.40,
                                                   mse = T)
# make one table of all three sizes of valdata
table_erc_mse_wide <- 
  left_join(table_erc_mse_40_wide,
            left_join(table_erc_mse_25_wide,
                      table_erc_mse_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_erc_mse_wide <- as.data.frame(table_erc_mse_wide)
colnames(table_erc_mse_wide) <- c("", "Scenario", "", 
                                  "", "40%", "",
                                  "", "25%", "",
                                  "", "10%", ""
)
table_erc_mse_wide <- rbind(c("", "", "",
                              "R", "SR", "E",
                              "R", "SR", "E",
                              "R", "SR", "E"), table_erc_mse_wide)
table_erc_mse_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                              "", "MSE", "",
                              "", "MSE", "",
                              "", "MSE", ""), table_erc_mse_wide)
# Create TeX table
table_erc_mse_wide_xtable <- print(xtable(table_erc_mse_wide,
                                          caption = caption,
                                          digits = 0),
                                   include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_erc_mse.txt")
writeLines(table_erc_mse_wide_xtable, file_con)
close(file_con)
















caption <-  
  c("Percentage bias and coverage in the estimated association between visceral adipose tissue and insulin resistance by application of standard regression calibration")
table7 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "perc_bias",
                   "cover")]
table7$perc_bias <- round(table7$perc_bias, 1)
table7$cover <- round(table7$cover * 100, 1)
table7$linear <- ifelse(table7$linear == 0, "no", "yes")

table_cc <- subset(table7, method == "complete_case")
table_cc <- table_cc[, -c("method")]
table_cc_10_wide <- subset_table_size_valdata(table_cc,
                                              0.10)
table_cc_25_wide <- subset_table_size_valdata(table_cc,
                                              0.25)
table_cc_40_wide <- subset_table_size_valdata(table_cc,
                                              0.40)
# make one table of all three sizes of valdata
table_cc_wide <- 
  left_join(table_cc_40_wide,
            left_join(table_cc_25_wide,
                      table_cc_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_cc_wide <- as.data.frame(table_cc_wide)
colnames(table_cc_wide) <- c("", "Scenario", "", 
                             "", "", "40%", "", "", "", 
                             "", "", "25%", "", "", "", 
                             "", "", "10%", "", "", "")
table_cc_wide <- rbind(c("", "", "",
                         "R", "SR", "E",
                         "R", "SR", "E", 
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E"), table_cc_wide)
table_cc_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                         "", "Bias", "", 
                         "", "Coverage", "",
                         "", "Bias", "",
                         "", "Coverage", "",
                         "", "Bias", "",
                         "", "Coverage", ""), table_cc_wide)
# Create TeX table
table_cc_wide_xtable <- print(xtable(table_cc_wide,
                                     caption = caption,
                                     digits = 0),
                              include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_ivs_bias_cover.txt")
writeLines(table_cc_wide_xtable, file_con)
close(file_con)











caption <-  
  c("Percentage bias and coverage in the estimated association between visceral adipose tissue and insulin resistance by application of validation regression calibration")
table8 <-
  sum_analysis[, c("sampling_strat",
                   "size_valdata",
                   "linear", 
                   "method",
                   "skewness",
                   "R_squared",
                   "perc_bias",
                   "cover")]
table8$perc_bias <- round(table8$perc_bias, 1)
table8$cover <- round(table8$cover * 100, 1)
table8$linear <- ifelse(table8$linear == 0, "no", "yes")

table_vrc <- subset(table8, method == "complete_case")
table_vrc <- table_vrc[, -c("method")]
table_vrc_10_wide <- subset_table_size_valdata(table_vrc,
                                              0.10)
table_vrc_25_wide <- subset_table_size_valdata(table_vrc,
                                              0.25)
table_vrc_40_wide <- subset_table_size_valdata(table_vrc,
                                              0.40)
# make one table of all three sizes of valdata
table_vrc_wide <- 
  left_join(table_vrc_40_wide,
            left_join(table_vrc_25_wide,
                      table_vrc_10_wide,
                      by = c("linear", "skewness", "R_squared")),
            by = c("linear", "R_squared", "skewness"))
# Change columnames 
table_vrc_wide <- as.data.frame(table_vrc_wide)
colnames(table_vrc_wide) <- c("", "Scenario", "", 
                             "", "", "40%", "", "", "", 
                             "", "", "25%", "", "", "", 
                             "", "", "10%", "", "", "")
table_vrc_wide <- rbind(c("", "", "",
                         "R", "SR", "E",
                         "R", "SR", "E", 
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E",
                         "R", "SR", "E"), table_vrc_wide)
table_vrc_wide <- rbind(c("Linear", "Skewness", "R-Squared",
                         "", "Bias", "", 
                         "", "Coverage", "",
                         "", "Bias", "",
                         "", "Coverage", "",
                         "", "Bias", "",
                         "", "Coverage", ""), table_vrc_wide)
# Create TeX table
table_vrc_wide_xtable <- print(xtable(table_vrc_wide,
                                     caption = caption,
                                     digits = 0),
                              include.rownames = FALSE)
file_con <- file("./results/tables/table_supp_vrc_bias_cover.txt")
writeLines(table_vrc_wide_xtable, file_con)
close(file_con)

