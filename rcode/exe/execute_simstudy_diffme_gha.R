#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Executive script that produces output of the simulation study -- diff
## lindanab4@gmail.com - 20200305
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/sim/run_sim.R")
args <- commandArgs(trailingOnly = TRUE)
args <- as.numeric(args)

##############################
# 1 - Run simulation study 
##############################
# Select datagen_scenarios and analysis_scenarios to be used
use_datagen_scenarios <- datagen_scenarios()[42,]
use_analysis_scenarios <- 
  analysis_scenarios()[analysis_scenarios()$size_valdata == 0.4,]
# Run simulation study
run_sim(rep = args[1],
        use_datagen_scenarios = use_datagen_scenarios,
        use_analysis_scenarios = use_analysis_scenarios)