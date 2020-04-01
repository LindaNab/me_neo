#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Process simulation output
## lindanab4@gmail.com - 20200317
#############################################################

##############################
# Some notes on where the *processed* data from .data/output will be saved
##############################
# After running the simulation, there exist 3 x 50 files in one of the 4 x 5 
# .data/output/ directories named 'S#_sampling_strat'. 
# These functions change var_beta to se_beta, add colnames to the .Rds files and 
# save the corresponding files to directories with the following structure:
# ./data/processed
# ├── size_valdata_10
# │ ├── method_complete_case <- S1_random,.., S50_uniform
# │ ├── ...
# │ └── method_reg_cal <- S1_random,.., S50_uniform
# ...
# └── size_valdata_50 
#   ├── method_complete_case <- S1_random,.., S50_uniform
#   ├── ...
#   └── method_reg_cal <- S1_random,.., S50_uniform

##############################
# 0 - Load librairies ----
##############################
library(data.table)
source(file = "./rcode/dgm/sim_scen.R")
source(file = "./rcode/sim/run_sim.R")
source(file = "./rcode/sim/create_data_dirs.R")

##############################
# 1 - Helper functions ----
##############################
# The function process_one_analysis_scenario saves .Rds files of the processed
# simulation output in .data/output of one analysis_scenario in .data/processed 
# holding the same structure
# FE: all 150 files in .data/output/size_valdata_10/method_complete_case are
# formatted and saved to 150 files in 
# .data/processed/size_valdata_10/method_complete_case
process_one_analysis_scenario <- function(analysis_scenario, 
                                          use_datagen_scenarios,
                                          output_dir,
                                          processed_dir){
  scen_nums <- use_datagen_scenarios[['scen_num']]
  files <- sapply(scen_nums, 
                  FUN = seek_output_file, 
                  analysis_scenario = analysis_scenario,
                  output_dir = output_dir)
  invisible(lapply(files,
            FUN = process_and_save_Rds, 
            output_dir = output_dir,
            processed_dir = processed_dir))
}
# Seeks the simulation output that belongs to the analysis_scenario and 
# datagen_scenario scen_num
seek_output_file <- function(analysis_scenario, 
                             scen_num,
                             output_dir){
  dir_name <- get_dir_name(analysis_scenario, data_dir = output_dir)
  file_name <- get_file_name(analysis_scenario, scen_num)
  file <- paste0(dir_name, "/", file_name)
}
# Formats a .Rds file from output_dir and saves in processed_dir
process_and_save_Rds <- function(file,
                                 output_dir,
                                 processed_dir){
  processed_file_name <- sub(output_dir, processed_dir, file)
  sim_output <- readRDS(file)
  processed_sim_output <- data.table(sim_output)
  colnames(processed_sim_output) <- c('beta', 
                                      'var_beta', 
                                      'size_valdata', 
                                      'R_squared',
                                      'seed')
  processed_sim_output <- sqrt_var_beta(processed_sim_output)
  processed_sim_output
  saveRDS(processed_sim_output, file = processed_file_name)
  message <- paste0(processed_file_name, " saved!")
  print(message)
}
# The function sqrt_var_beta takes the sqrt of variance of beta (standard error)
sqrt_var_beta <- function(datatable_sim_output){
  datatable_sim_output[ , 'var_beta' := 
                          sqrt(datatable_sim_output[['var_beta']])]
  colnames(datatable_sim_output)[
    colnames(datatable_sim_output) == "var_beta"] <- "se_beta"
  datatable_sim_output
}
##############################
# 2 - Workhorse ----
##############################
process_sim_output <- function(use_datagen_scenarios = datagen_scenarios(),
                               use_analysis_scenarios = analysis_scenarios(),
                               output_dir = "./data/output",
                               processed_dir = "./data/processed"){
  # creates directories where .Rds filse will be saved
  levels <- list(
    "size_valdata" = 
      unique(use_analysis_scenarios[['size_valdata']]) * 100,
    "method" = 
      unique(use_analysis_scenarios[['method']])
  )
  create_data_dirs(data_dir = processed_dir, levels = levels)
  # apply function process_one_analysis_scenario on all analysis_scenarios with
  # and datagen_scenarios scen_nums
  invisible(apply(use_analysis_scenarios,
            1,
            FUN = process_one_analysis_scenario,
            use_datagen_scenarios = use_datagen_scenarios,
            output_dir = output_dir,
            processed_dir = processed_dir))
}