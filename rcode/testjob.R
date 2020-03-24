#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Executive script
## lindanab4@gmail.com - 20200305
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/sim/run_sim.R")

##############################
# 1 - Run simulation study 
##############################
# Select datagen_scenarios and analysis_scenarios to be used
#use_datagen_scenarios <- datagen_scenarios()[16,]
#use_analysis_scenarios <- analysis_scenarios()[c(46, 52),]
use_datagen_scenarios <- datagen_scenarios()
use_analysis_scenarios <- analysis_scenarios()

# not run
# run_sim()
run_sim(rep = 5, 
        use_datagen_scenarios = use_datagen_scenarios, 
        use_analysis_scenarios = use_analysis_scenarios)