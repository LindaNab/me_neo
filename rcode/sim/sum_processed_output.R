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

# To do: change eval_param in without _mcse and add later on
# n_sim can get _mcse
# n_valdata is the only param that not from simsum
# To do 2: add notes
eval_param <- function(){
  eval_param <- c("bias", 
                  "bias_mcse",
                  "perc_bias",
                  "mse", 
                  "mse_mcse",
                  "cover",
                  "cover_mcse", 
                  "modelse",
                  "modelse_mcse", 
                  "empse",
                  "empse_mcse",
                  "n_valdata",
                  "n_sim")
}
# creates data.table that will be filled later on
init_summary <- function(sim_scen_levels,
                         use_eval_param){
  size <- NROW(sim_scen_levels)
  out_datatable <- data.table(sim_scen_levels)
  out_datatable[, (use_eval_param) := numeric(size)]
  out_datatable[]
}
save_summary <- function(summary, summarised_dir){
  output_file <- paste0(summarised_dir, "/summary.Rds")
  saveRDS(summary, file = output_file)
}
fill_summary <- function(summary,
                         use_analysis_scenarios,
                         use_datagen_scenarios){
  for(j in 1:NROW(use_analysis_scenarios)){
    for(i in 1:NROW(use_datagen_scenarios)){
      fill_one_row_of_summary(summary,
                              analysis_scenario = use_analysis_scenarios[j,],
                              datagen_scenario = use_datagen_scenarios[i,])
    }
  }
}

fill_one_row_of_summary <- function(summary,
                                    analysis_scenario,
                                    datagen_scenario){
  file <- seek_file(analysis_scenario, 
                    datagen_scenario, 
                    data_dir = "./data/processed")
  processed_output <- readRDS(file = file)
  simsum <- rsimsum::simsum(data = processed_output,
                            estvarname = "beta",
                            true = 0.01,
                            se = "se_beta")
  sim_scen <- cbind(datagen_scenario, analysis_scenario)
  sim_scen <- data.table(sim_scen)
  # get row_number of this sim_scen in summary
  row_num <- summary[sim_scen, on = colnames(sim_scen), which = TRUE]
  stats <- c("bias", "mse", "cover", "modelse", "empse")
  for(i in 1:NROW(stats)){
    fill_row_with_stat(row_num, summary, simsum, stats[i])
  }
  summary[row_num, n_valdata := mean(processed_output$size_valdata)]
  summary[row_num, perc_bias := (summary[row_num, bias] / 0.01) * 100]
  simsum_table <- get_data(simsum)
  summary[row_num, n_sim := simsum_table[simsum_table$stat == "nsim",]$est]
  print(paste0(file, " summarized!"))
  summary[]
}
# uses simsum to fill the row of summary with the summarised sim params
fill_row_with_stat <- function(row_num, summary, simsum, stat){
  simsum_table <- rsimsum::get_data(simsum)
  args <- c(simsum_table[simsum_table$stat == stat, ])
  add_value <- function(stat, est, mcse){
    stat_mcse <- paste0(stat, "_mcse")
    summary[row_num, (stat) := est]
    summary[row_num, (stat_mcse) := mcse]}
  do.call(add_value, args)
}

##
# Workhorse
##
summarize_sim <- function(use_analysis_scenarios = analysis_scenarios(),
                          use_datagen_scenarios = datagen_scenarios(),
                          processed_dir = "./data/processed",
                          summarised_dir = "./data/summarised"){
  # summary will inculde all different analysis_scenarios times the different 
  # datagen_scenarios
  sim_scen_levels <- merge(use_analysis_scenarios, 
                           use_datagen_scenarios, 
                           by = NULL)
  # init data.table for summary
  summary <- init_summary(sim_scen_levels, 
                          use_eval_param = eval_param())
  # fill summary
  fill_summary(summary,
               use_analysis_scenarios = use_analysis_scenarios,
               use_datagen_scenarios = use_datagen_scenarios)
  # save summary
  save_summary(summary, summarised_dir)
}
