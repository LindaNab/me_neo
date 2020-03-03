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

simulation <- function(S = 5000,
                       datagen_scenario,
                       analyse_scenario){
  for(i in 1:S){
    data <- gen_data(lambda = datagen_scenarios()[26, 'lambda'],
                     tau = datagen_scenarios()[26, 'tau'],
                     heteroscedastic = datagen_scenarios()[26, 'heteroscedastic'])
    # mapply(analyse_data, 
    #        analyse_scenarios()$size_valdata, 
    #        analyse_scenarios()$sampling_strat,
    #        analyse_scenarios()$method,
    #        MoreArgs = list(data = data))
    analyse_data(data = data,
                 size_valdata = analyse_scenarios()[1, "size_valdata"],
                 sampling_strat = analyse_scenarios()[1, "sampling_strat"], 
                 method = analyse_scenarios()[1, "method"])
  }
}
