#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 0 - Simulation scenarios ----
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies ----
##############################

############################## 
# 1 - Helper Functions ----
##############################
# by increasing tau, we add more measurement error
# var_VAT = 2075, approximately the variance of VAT (estimated by generating a
# data set of size 1e7)
calc_tau <- function(R_squared, theta = 0.16, var_VAT = 2075){
  tau_squared <- ((1 - R_squared) * theta^2 * var_VAT) / R_squared
  return(sqrt(tau_squared))
}
# by changing lambda, we change the skewness of the data
calc_lambda <- function(skewness, k = 6.1){
  lambda <- 4 / (k * skewness^2)
  return(lambda)
}

############################## 
# 2 - Function that creates data.frame with 50 scenarios used to generate data
##############################
datagen_scenarios <- function(){
  skewness <- c(0.1, 1, 2, 4, 6)
  lambdas <- sapply(skewness, calc_lambda)
  R_squared <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  taus <- sapply(R_squared, calc_tau)
  heteroscedastic <- c(TRUE, FALSE)
  
  # data.frame with simulation scenarios
  datagen_scenarios <- expand.grid(lambdas, 
                                   taus, 
                                   heteroscedastic)
  datagen_scenarios$scen_num <- c(1:NROW(datagen_scenarios))
  colnames(datagen_scenarios) <- c("lambda", 
                                   "tau", 
                                   "heteroscedastic",
                                   "scen_num")
  return(datagen_scenarios)}
############################## 
# 2 - Function that creates data.frame with 50 scenarios, differently analysing
# the data + makes folder structure
##############################
create_dir <- function(folder_name){
  if(!file.exists(folder_name)){
    dir.create(folder_name)
  }
}
give_folder_structure <- function(in_folder, prefix, folder_names){
  char <- as.vector(sapply(in_folder, 
                           function(x) paste0(x, prefix, folder_names),
                           USE.NAMES = F))
  char
}

analyse_scenarios <- function(){
  size_valdata <- c(0.10, 0.25, 0.40, 0.50)
  sampling_strat <- c("random", "uniform", "extremes")
  method <- c("complete_case",
              "naive",
              "reg_cal",
              "efficient_reg_cal",
              "inadm_reg_cal")
  size_valdata_percent <- 100 * size_valdata
  # create directories if not already there
  folders_size_valdata <- give_folder_structure(
    in_folder = "./data/output", 
    prefix = "/size_valdata_",
    folder_names = size_valdata_percent
    )
  sapply(folders_size_valdata, create_dir)
  folders_sampling_strat <- give_folder_structure(
    in_folder = folders_size_valdata,
    prefix = "/sampling_strat_",
    folder_names = sampling_strat
  )
  sapply(folders_sampling_strat, create_dir)
  folders_method <- give_folder_structure(
    in_folder = folders_sampling_strat,
    prefix = "/method_",
    folder_names = method
  )
  sapply(folders_method, create_dir)
  # data.frame with simulation scenarios
  analyse_scenarios <- expand.grid(size_valdata, 
                                   sampling_strat, 
                                   method)
  colnames(analyse_scenarios) <- c("size_valdata", 
                                   "sampling_strat", 
                                   "method")
  return(analyse_scenarios)}
