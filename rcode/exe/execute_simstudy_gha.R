#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Executive script that produces output of the simulation study
## lindanab4@gmail.com - 20200305
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/sim/run_sim.R")
args <- commandArgs(trailingOnly = TRUE)
args <- as.numeric(args)
# args <- scan(args[1])
# print(input_from_file)

##############################
# 1 - Run simulation study 
##############################
# Select datagen_scenarios and analysis_scenarios to be used
use_datagen_scenarios <- datagen_scenarios()[args[1] + 1,] # row 1 is scen_num 0
use_analysis_scenarios <- subset(analysis_scenarios(), sampling_strat == "uniform")
# Run simulation study
run_sim(rep = args[2],
        use_datagen_scenarios = use_datagen_scenarios,
        use_analysis_scenarios = use_analysis_scenarios)