#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Summarize processed output
## lindanab4@gmail.com - 20200317
#############################################################

##############################
# Some notes on the format of the *summarised* data from .data/proccessed 
##############################
# The table contains #datagen_scenarios x #sampling_strat x #method x 
# #size_valdata = 50 x 3 x 5 x 4 = 3000 rows, in this order:
# size_valdata, method, sampling_strat, heterosc, expl_var, skewness, scen_num
# 0.10          cc      random          0          0.1      0.2       S1
# 0.10          cc      random          0          ..       ..        ..
# 0.10          cc      random          0          6        0.8       S25
# 0.10          cc      random          1          0.1      0.2       S26
# 0.10          cc      random          1          ..       ..        ..
# 0.10          cc      random          1          6        0.8       S50
# ..            ..      ..              ..         ..       ..        ..
# 0.10          cc      uniform         0          0.1      0.2       S1
# 0.10          cc      uniform         0          ..       ..        ..
# 0.10          cc      uniform         0          6        0.8       S25
# 0.10          cc      uniform         1          0.1      0.2       S26
# 0.10          cc      uniform         1          ..       ..        ..
# 0.10          cc      uniform         1          6        0.8       S50
# ..            ..      ..              ..                            ..
# 0.50          irc     uniform         1          6        0.8       S50
# with the following columns (information):
# bias, bias_mcse, mse, mse_mcse, cov, cov_mcse, 
# modse, modse_mcse, empse, empse_mcse, n_valdata, n_sim

##############################
# 0 - Load librairies ----
##############################
library(data.table)
library(rsimsum)
source(file = "./rcode/sim/run_sim.R")

# next: write function that summarises all scen_nums for one analysis_scenario
# bind results using rbindlist?

# second: write function that does this for al analysis_scenarios

# This function summarises one sim scenario (analysis_scenario/datagen_scenario)
summary_sim_scenario <- function(analysis_scenario,
                                 scen_num, 
                                 processed_dir = "./data/processed"){
  summary <- create_datatable_for_summary()
  file <- seek_file(analysis_scenario, 
                    scen_num, 
                    data_dir = processed_dir)
  processed_output <- readRDS(file)
  fill_summary_with_scen_info(summary, 
                              analysis_scenario, 
                              scen_num)
  simsum <- rsimsum::simsum(data = processed_output,
                            estvarname = "beta",
                            true = 0.01,
                            se = "se_beta")
  stats <- c("bias", "mse", "cover", "modelse", "empse")
  for(i in 1:NROW(stats)){
    fill_summary_with_stat(summary, simsum, stats[i])
  }
  summary[, n_valdata := mean(processed_output$size_valdata)]
  simsum_table <- get_data(simsum)
  summary[, n_sim := simsum_table[simsum_table$stat == "nsim",]$est]
}
fill_summary_with_scen_info <- function(summary, 
                                        analysis_scenario,
                                        scen_num){
  analysis_info <- c("size_valdata", 
                     "method", 
                     "sampling_strat")
  datagen_scenario <- datagen_scenarios()[datagen_scenarios()$scen_num == 
                                            scen_num, ]
  datagen_info <- c("heteroscedastic",
                    "R_squared", 
                    "skewness",
                    "scen_num")
  scen_info <- function(info, scen){
    for(i in 1:NROW(info)){
      summary[, info[i] := scen[[info[i]]]]
    }
  }
  scen_info(analysis_info, analysis_scenario)
  scen_info(datagen_info, datagen_scenario)
}
# perhaps I can simplify this using datagen_info and analysis_info??
create_datatable_for_summary <- function(){
  out_datatable <- data.table('size_valdata' = numeric(1),
                              'method' = numeric(1),
                              'sampling_strat' = numeric(1),
                              'heteroscedastic' = numeric(1),
                              'R_squared' = numeric(1),
                              'skewness' = numeric(1),
                              'scen_num' = numeric(1),
                              'bias' = numeric(1), 
                              'bias_mcse' = numeric(1), 
                              'mse' = numeric(1),
                              'mse_mcse' = numeric(1),
                              'cover' = numeric(1),
                              'cover_mcse' = numeric(1), 
                              'modelse' = numeric(1),
                              'modelse_mcse' = numeric(1), 
                              'empse' = numeric(1),
                              'empse_mcse' = numeric(1),
                              'n_valdata' = numeric(1),
                              'n_sim'= numeric(1))}
fill_summary_with_stat <- function(summary, simsum, stat){
  simsum_table <- rsimsum::get_data(simsum)
  args <- c(simsum_table[simsum_table$stat == stat, ])
  add_value <- function(stat, est, mcse){
    stat_mcse <- paste0(stat, "_mcse")
    summary[, (stat) := est]
    summary[, (stat_mcse) := mcse]}
  do.call(add_value, args)
}
seek_file <- function(analysis_scenario, 
                      scen_num, 
                      data_dir){
  dir_name <- get_dir_name(analysis_scenario, 
                           data_dir = processed_dir)
  file_name <- get_file_name(analysis_scenario, scen_num)
  file <- paste0(dir_name, "/", file_name)
}
