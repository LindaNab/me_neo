#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 1 - Run simulation study
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./src/dgm/0_sim_scen.R")
source(file = "./src/dgm/1_gen_data.R")
source(file = "./src/data_analysis/6_analyse_data.R")

############################## 
# 1 - Helper Functions ----
##############################

datagen_scenario <- datagen_scenarios()[2,]
analyse_scenario <- analyse_scenarios()[1,]
seed <- 1
result <- results[1,]

seek_folder <- function(analyse_scenario){
  output_folder <- "./data/output"
  size_valdata <- 100 * as.numeric(analyse_scenario['size_valdata'])
  sampling_strat <- analyse_scenario['sampling_strat']
  method <- analyse_scenario['method']
  paste0(output_folder, 
         "/size_valdata_", size_valdata, 
         "/sampling_strat_", sampling_strat,
         "/method_", method)
}
save_result <- function(result){
  scen_num <- result['scen_num']
  beta <- as.numeric(result['beta'])
  var_beta <- as.numeric(result['var_beta'])
  folder <- result['folder']
  file <- paste0(folder, "/S_", scen_num, ".rds")
  saveRDS(c(beta, var_beta, seed), file = file) # needs something to append results
}

perform_one_run <- function(seed, 
                            datagen_scenario){
  data <- gen_data(lambda = as.numeric(
                     datagen_scenario['lambda']),
                   tau = as.numeric(
                     datagen_scenario['tau']),
                   heteroscedastic = as.numeric(
                     datagen_scenario['heteroscedastic']),
                   seed = seed)
  scen_num <- as.numeric(datagen_scenario['scen_num'])
  results <- apply(analyse_scenarios(), 1, FUN = analyse_data, data = data)
  results <- as.data.frame(t(rbind(scen_num, 
                                   results, 
                                   apply(analyse_scenarios(), 1, seek_folder),
                                   seed)))
  colnames(results) <- c("scen_num", "beta", "var_beta", "folder", "seed")
  apply(results, 1, save_result)
}

simulation <- function(S = 5000,
                       datagen_scenario,
                       analyse_scenario){
  for(i in 1:S){
    perform_one_run(datagen_scenario)
  }
}
