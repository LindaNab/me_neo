#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Analyse the data, it samples validation data of the correct size, 
## lindanab4@gmail.com - 20200303
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
# helper scripts
source(file = "./rcode/analyses/complete_case.R")
source(file = "./rcode/analyses/naive.R")
source(file = "./rcode/analyses/reg_cal.R")
source(file = "./rcode/analyses/efficient_reg_cal.R")
source(file = "./rcode/analyses/inadm_reg_cal.R")
source(file = "./rcode/analyses/sample_valdata.R")

############################## 
# 1 - Function that analyses the data using with the appropriate method
##############################
get_result <- function(data, 
                       method){
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
# 2 - Work horse that samples data and pulls the result
##############################
analyse_data <- function(analyse_scenario, 
                         data){
  size_valdata <- as.numeric(analyse_scenario['size_valdata'])
  sampling_strat <- analyse_scenario['sampling_strat']
  method <- analyse_scenario['method']
  data <- select_valdata(data = data, 
                         size_valdata = size_valdata, 
                         use_variable = "WC",
                         sampling_strat = sampling_strat)
  result <- get_result(data, method)
  result <- c(result,
              "n_valdata" = NROW(data[data$in_valdata == 1,]))
  result
}