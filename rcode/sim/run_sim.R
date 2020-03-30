#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Run simulation study
## lindanab4@gmail.com - 20200221
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

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/dgm/sim_scen.R")
source(file = "./rcode/dgm/gen_data.R")
source(file = "./rcode/analyses/analyse_data.R")
source(file = "./rcode/sim/create_data_dirs.R")

############################## 
# 1 - Helper Functions ----
##############################
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
# save the output in dir_name/file_name
save_result <- function(result){
  beta <- as.numeric(result['beta'])
  var_beta <- as.numeric(result['var_beta'])
  n_valdata <- as.numeric(result['n_valdata'])
  seed <- as.numeric(result['seed'])
  dir_name <- result[['dir_name']]
  file_name <- result[['file_name']]
  file <- paste0(dir_name, "/", file_name) 
  # append new result to old results if file exists
  if (file.exists(file)){
    con <- file(file)
    while (isOpen(con)){
      Sys.sleep(2)
    }
    open(con)
    results_in_file <- readRDS(file)
    new_results <- rbind(results_in_file, c(beta, var_beta, n_valdata, seed))
    rownames(new_results) <- NULL
    saveRDS(new_results, file = file)
    close(con)
    } else{ #create new file
  saveRDS(c(beta, var_beta, n_valdata, seed), file = file)}
  message <- paste0(file, " saved!")
  print(message)
}

##############################
# 2 - Seed ---
##############################
get_seeds <- function(rep){
  set.seed(20200305)
  n_seed <- NROW(datagen_scenarios()) * rep # 51 datagen_scenarios() 
                                            # and 5000 replications per scenario
  seed <- sample(1:1e8, 
                 size = n_seed, 
                 replace = FALSE)
}

############################## 
# 3 - Simulation study in steps
##############################
# Perform one run, i.e.: for one datagen_scenario (of the in total 50, see 
# datagen_scenarios(): S1-S50) generate data, and perform the 60 different 
# analyses (see analysis_scenarios())
perform_one_run <- function(seed, 
                            datagen_scenario,
                            use_analysis_scenarios,
                            output_dir){
  # generate data
  data <- gen_data(lambda = datagen_scenario[['lambda']],
                   theta = datagen_scenario[['theta']],
                   tau = datagen_scenario[['tau']],
                   heteroscedastic = datagen_scenario[['heteroscedastic']],
                   seed = seed)
  scen_num <- datagen_scenario[['scen_num']]
  # analyse the data using use_analysis_scenarios
  results <- apply(use_analysis_scenarios, 
                   1, 
                   FUN = analyse_data, 
                   data = data)
  results <- as.data.frame(t(rbind(results, 
                                   seed,
                                   apply(use_analysis_scenarios, 
                                         1, 
                                         FUN = get_dir_name,
                                         data_dir = output_dir),
                                   apply(use_analysis_scenarios, 
                                         1, 
                                         FUN = get_file_name, 
                                         scen_num = scen_num)
                                   )))
  colnames(results) <- c("beta", 
                         "var_beta",
                         "n_valdata", 
                         "seed", 
                         "dir_name", # directory were results will be saved
                         "file_name") # name of the .Rds file
  apply(results, 1, save_result)
}
# Repeat 'perform_one_run' rep times, for one specific datagen_scenario (see 
# datagen_scenarios(): S1-S50). FE: for S1 of datagen_scenarios()
sim_one_datagen_scenario <- function(datagen_scenario,
                                     use_analysis_scenarios,
                                     rep,
                                     seeds,
                                     output_dir){
  scen_num <- as.numeric(datagen_scenario['scen_num'])
  for(i in 1:rep){
    perform_one_run(seed = seeds[(rep * scen_num + i)], # for now, seed for 
                    # each datagen_scenario() is: S0: 1 - 5000, S1: 5001 - 10000
                    # etc. if rep = 5000
                    datagen_scenario = datagen_scenario,
                    use_analysis_scenarios = use_analysis_scenarios,
                    output_dir = output_dir)
    print(i)
  }
}
# Workhorse of the simulation study. For each of the datagen_scenarios(), rep 
# data sets will be generated, and analysed using analysis_scenarios(). The 
# default will generate 20 * 150 files each including 5000 rows. 
run_sim <- function(rep = 5000, 
                    use_datagen_scenarios = datagen_scenarios(),
                    use_analysis_scenarios = analysis_scenarios(),
                    seeds = get_seeds(rep),
                    output_dir = "./data/output"){
  print(seeds)
  # levels of data_dirs (see the described structure above)
  levels <- list(
    "size_valdata" = 
      unique(use_analysis_scenarios[['size_valdata']]) * 100,
    "method" = 
      unique(use_analysis_scenarios[['method']])
  )
  create_data_dirs(levels = levels)
  invisible(apply(use_datagen_scenarios, 
                  1, 
                  FUN = sim_one_datagen_scenario, 
                  use_analysis_scenarios = use_analysis_scenarios,
                  rep = rep,
                  seeds = seeds,
                  output_dir = output_dir))
}
