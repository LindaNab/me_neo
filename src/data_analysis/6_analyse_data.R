#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 5 Perform the data analysis
## lindanab4@gmail.com - 20200303
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./src/data_analysis/1_complete_case.R")
source(file = "./src/data_analysis/2_naive.R")
source(file = "./src/data_analysis/3_reg_cal.R")
source(file = "./src/data_analysis/4_efficient_reg_cal.R")
source(file = "./src/data_analysis/5_inadm_reg_cal.R")
source(file = "./src/dgm/2_sample_valdata.R")

############################## 
# 1 - Function that analyses the data using with the appropriate method
##############################
get_result <- function(data, method){
  if (method == "complete_case"){
    result <- complete_case(data)
  }
  else if (method == "naive"){
    result <- naive(data)
  }
  else if (method == "reg_cal"){
    result <- reg_cal(data)
  }
  else if (method == "efficient_reg_cal"){
    result <- efficient_reg_cal(data)
  }
  else if (method == "inadm_reg_cal"){
    result <- inadm_reg_cal(data)
  }
  result
}

############################## 
# 1 - Work horse that samples data and pulls the result
##############################
analyse_data <- function(analyse_scenario, data){
  size_valdata <- as.numeric(analyse_scenario['size_valdata'])
  sampling_strat <- analyse_scenario['sampling_strat']
  method <- analyse_scenario['method']
  data <- select_valdata(data = data, 
                         size_valdata = size_valdata, 
                         use_variable = "WC",
                         sampling_strat = sampling_strat)
  result <- get_result(data, method)
  result
}