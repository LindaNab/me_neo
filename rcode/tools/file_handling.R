#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Functions that handle the file structure of the output of 
## the simulation study
## lindanab4@gmail.com - 20200403
#############################################################

##############################
# Some notes on where the simulation output will be saved
##############################
# The output of each run (beta, var_beta, n_valdata, seed) will be saved as 
# follows: for each size of the validation data (10, 25, 40 or 50 percent), a 
# directory is created in ./data/output. Within those directories, directories
# are created for each method (cc, eff_reg_cal, inadm_reg_cal, naive, reg_cal)
# This results in 20 different output folders. In each folder, 150 RDS files
# will be saved: the 50 datagen_scenarios() will be named S1-S50, for each 
# scenario, 3 RDS files will be created, with the suffix _random, _uniform or 
# _extremes. In total there will be 20 folders, each including 150 files (3 
# times 50). The structure is displayed here:
# ./data/output
# ├── size_valdata_10
# │ ├── method_complete_case <- S1_random,.., S1_extremes,.., S50_extremes
# │ ├── ...
# │ └── method_reg_cal <- S1_random,.., S1_extremes,.., S50_extremes
# ...
# └── size_valdata_50 
#   ├── method_complete_case <- S1_random,.., S1_extremes,.., S50_extremes
#   ├── ...
#   └── method_reg_cal <- S1_random,.., S1_extremes,.., S50_extremes

# Gets the directory name where the output of one simulation run will be saved
# fe: ./data/output/size_valdata_10/method_random/
get_dir_name <- function(analysis_scenario, 
                         data_dir){
  size_valdata <- 100 * as.numeric(analysis_scenario[['size_valdata']])
  method <- analysis_scenario[['method']]
  paste0(data_dir, 
         "/size_valdata_", 
         size_valdata,
         "/method_", 
         method)
}
# Gets the name of the .Rds file where the ouput will be saved
# fe: S1_random,.., S2_extremes etc
get_file_name <- function(analysis_scenario, scen_num){
  sampling_strat <- analysis_scenario[['sampling_strat']]
  paste0("S",
         scen_num,
         "_",
         sampling_strat,
         ".Rds")
}
# seeks file of analysis_scenario for given datagen_scenario
seek_file <- function(analysis_scenario, 
                      datagen_scenario = NULL, 
                      scen_num = NULL,
                      data_dir){
  # use datagen_scenario if given, else use scen_num
  if(!is.null(datagen_scenario)){
    scen_num <- datagen_scenario[['scen_num']]
  }
  if(is.null(scen_num) & is.null(datagen_scenario)){
    stop("scen_num and datagen_scenario are both NULL")
  }
  dir_name <- get_dir_name(analysis_scenario, 
                           data_dir = data_dir)
  file_name <- get_file_name(analysis_scenario, scen_num)
  file <- paste0(dir_name, "/", file_name)
  file
}