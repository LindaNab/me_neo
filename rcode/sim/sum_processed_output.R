#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Summarize processed output
## lindanab4@gmail.com - 20200317
#############################################################

# This script summarizes all processed output to one data.table:
# scen_num, size_valdata, method, sampling_strat, bias, bias_mcse, 
# empse, empse_mcse, 

##############################
# 0 - Load librairies ----
##############################
library(data.table)
library(rsimsum)

sum_processed_output_one_dir <- function(processed_dir, 
                                         scen_nums = paste0("S", 1:50)){
  
}
# This function summarises one processed .Rds file
file <- "./data/processed/size_valdata_10/method_complete_case/S1.Rds"
summarise_Rds <- function(file){
  out <- data.table()
  processed_output <- readRDS(file)
  simsum <- rsimsum::simsum(data = processed_output,
                            estvarname = "beta",
                            true = 0.01,
                            se = "se_beta", 
                            methodvar = "sampling_strat",
                            ref = "random")
  simsum_table <- get_data(simsum)
  methods <- levels(simsum_table$sampling_strat)
  out_datatable <- create_datatable_summary(methods)
}

create_datatable_summary <- function(methods){
  n_methods <- length(methods)
  out_datatable <- data.table('method' = methods,
                              'n_sim' = numeric(n_methods),
                              'bias' = numeric(n_methods), 
                              'bias_mcse' = numeric(n_methods), 
                              'empse' = numeric(n_methods),
                              'empse_mcse' = numeric(n_methods), 
                              'mse' = numeric(n_methods),
                              'mse_mcse' = numeric(n_methods),
                              'coverage' = numeric(n_methods),
                              'coverage_mcse' = numeric(n_methods))}
fill_datatable_summary <- function(methods){
  
}
