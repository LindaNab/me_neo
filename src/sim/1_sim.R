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

############################## 
# 1 - Helper Functions ----
##############################
simulation <- function(S = 5000,
                       scenario){
  for(i in 1:S){
    data <- gen_data(lambda = scenarios()[126, 'lambda'],
                     tau = scenarios()[126, 'tau'],
                     heteroscedastic = scenarios()[126, 'heteroscedastic'])
    for(sampling_strat in c("Random", "Uniform", "Extremes")){
      data <- select_valdata(data,
                     use_variable = "WC", 
                     size_valdata = scenarios()[126, 'size_valdata'],
                     sampling_strat = sampling_strat)
    }
  }
}
