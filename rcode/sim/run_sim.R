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
# This results in 20 different output folders. In each folder, 123 RDS files
# will be saved: the 41 datagen_scenarios() will be named S0-S40, for each 
# scenario, 3 RDS files will be created, with the suffix _random, _uniform or 
# _extremes. In total there will be 20 folders, each including 123 files (3 
# times 41). The structure is displayed here:
# ./data/output
# ├── size_valdata_10
# │ ├── method_complete_case <- S0_random,.., S1_extremes,.., S40_extremes
# │ ├── ...
# │ └── method_reg_cal <- S0_random,.., S1_extremes,.., S40_extremes
# ...
# └── size_valdata_50 
#   ├── method_complete_case <- S0_random,.., S1_extremes,.., S40_extremes
#   ├── ...
#   └── method_reg_cal <- S0_random,.., S1_extremes,.., S40_extremes

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/dgm/sim_scen.R")
source(file = "./rcode/dgm/gen_data.R")
source(file = "./rcode/analyses/analyse_data.R")
source(file = "./rcode/tools/create_data_dirs.R")
source(file = "./rcode/tools/file_handling.R")

############################## 
# 1 - Helper Functions ----
##############################
# save the output in dir_name/file_name
save_result <- function(result){
  beta <- as.numeric(result['beta'])
  var_beta <- as.numeric(result['var_beta'])
  n_valdata <- as.numeric(result['n_valdata'])
  R_squared <- as.numeric(result['R_squared'])
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
    new_results <- rbind(results_in_file, 
                         c(beta, var_beta, n_valdata, R_squared, seed))
    rownames(new_results) <- NULL
    saveRDS(new_results, file = file)
    close(con)
    } else{ #create new file
  saveRDS(c(beta, var_beta, n_valdata, R_squared, seed), file = file)}
  message <- paste0(file, " saved!")
  print(message)
}

##############################
# 2 - Seed ---
##############################
get_seeds <- function(rep){
  set.seed(20200305)
  n_seed <- NROW(datagen_scenarios()) * rep # 41 datagen_scenarios() 
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
  print(paste0("Perform_one_run with seed: ", seed))
  # generate data
  data <- gen_data(lambda = datagen_scenario[['lambda']],
                   theta = datagen_scenario[['theta']],
                   tau = datagen_scenario[['tau']],
                   linear = datagen_scenario[['linear']],
                   differential = datagen_scenario[['differential']],
                   seed = seed)
  scen_num <- datagen_scenario[['scen_num']]
  # analyse the data using use_analysis_scenarios
  results <- apply(use_analysis_scenarios, 
                   1, 
                   FUN = analyse_data, 
                   data = data,
                   seed = seed)
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
  results <- cbind(results, "R_squared" = get_R_squared(data))
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
    seed = seeds[(rep * scen_num + i)]
    perform_one_run(seed = seed, # for now, seed for 
                    # each datagen_scenario() is: S0: 1 - 5000, S1: 5001 - 10000
                    # etc. if rep = 5000
                    datagen_scenario = datagen_scenario,
                    use_analysis_scenarios = use_analysis_scenarios,
                    output_dir = output_dir)
    print(paste0("Run #", i, " with seed: ", seed))
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