#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Process simulation output
## lindanab4@gmail.com - 20200317
#############################################################

# After running the simulation, there exist 3 x 50 files in one of the 4 x 5 
# .data/output/ directories named 'S#_sampling_strat'. 
# These functions combine the .Rds files of the 3 different sampling strategies,
# change var_beta to se_beta and save the corresponding files with the following
# structure:
# ./data/processed
# ├── size_valdata_10
# │ ├── method_complete_case <- S1,.., S50
# │ ├── ...
# │ └── method_reg_cal <- S1,.., S50
# ...
# └── size_valdata_50 
#   ├── method_complete_case <- S1,.., S50
#   ├── ...
#   └── method_reg_cal <- S1,.., S50

##############################
# 0 - Load librairies ----
##############################
library(data.table)
source(file = "./rcode/sim/create_data_dirs.R")

##############################
# 1 - Helper functions ----
##############################
# The function process_sim_output_one_dir saves .Rds files of the combined 
# simulation output of one output_dir in .data/processed holding the same
# structure
# FE: all 150 files in .data/output/size_valdata_10/method_complete_case are 
# combined, formatted and saved to 50 files in 
# .data/processed/size_valdata_10/method_complete_case
process_sim_output_one_dir <- function(output_dir,
                                       scen_nums = paste0("S", 1:50)){
  # the directory where processed files will be saved has the same structure as 
  # the output directory: .data/processed/...
  processed_dir <- sub("output", "processed", output_dir)
  # create list with the combined results of all scen_nums
  combined_results <- lapply(scen_nums, 
                             FUN = combine_Rds, # combines Rds files samplstrat 
                             output_dir = output_dir)
  # change var_beta to se_beta
  combined_results_se <- lapply(combined_results, 
                                FUN = sqrt_var_beta) # creates se_beta column
  # change name, needed save to correct directory
  names(combined_results_se) <- scen_nums
  # Save files to processed_dir
  for(i in 1:length(combined_results_se)){
    file = paste0(processed_dir, names(combined_results_se[i]), ".Rds")
    saveRDS(combined_results_se[[i]], file)
    message <- paste0(file, " saved!")
    print(message)
  }
}
# combine_Rds outputs a data.table that combines the files of one scenario and 
# adds a column sampling_strat to indicate what sampling strategy was used.
# FE: S1_random.Rds, S1_extremes.Rds, S1_uniform.Rds in 
# output data in .output/data/size_valdata/method_complete_case will be combined 
# to one data.table.
combine_Rds <- function(scen_num,
                        output_dir,
                        sampling_strats = 
                          levels(analysis_scenarios()$sampling_strat)){
  file_names <- paste0(scen_num, 
                       "_",
                       sampling_strats,
                       ".Rds")
  file <- paste0(output_dir, file_names)
  sim_output <- lapply(file, readRDS)
  names(sim_output) <- sampling_strats
  datatable_sim_output <- lapply(sim_output, as.data.table)
  for(i in 1:3){
    datatable_sim_output[[i]] <- cbind(datatable_sim_output[[i]], 
                                       names(datatable_sim_output[i]))}
  combined_sim_output <- rbindlist(datatable_sim_output)
  colnames(combined_sim_output) <- c("beta", 
                                     "var_beta", 
                                     "size_valdata", 
                                     "seed", 
                                     "sampling_strat")
  combined_sim_output
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
# 1 - Workhorse ----
##############################
process_sim_output <- function(output_dir = "./data/output/",
                               scen_nums = paste0("S", 1:50)){
  processed_dir <- sub("output", "processed", output_dir)
  # Create directories where processed .Rds files will be saved
  levels <- list("size_valdata" = 
      unique(analysis_scenarios()$size_valdata) * 100,
                 "method" = 
      levels(analysis_scenarios()$method))
  create_data_dirs(data_dir = processed_dir,
                   levels = levels)
  dir_paths_output <- list_data_dirs(data_dir = output_dir,
                                     levels = levels)[[2]]
  dir_paths_output <- paste0(dir_paths_output, "/")
  invisible(sapply(dir_paths_output, 
                   FUN = process_sim_output_one_dir, 
                   scen_nums = scen_nums))
}