#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Executive script to run + process + summarise simulation study
## lindanab4@gmail.com - 20200305
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/sim/run_sim.R")
source(file = "./rcode/sumsim/process_sim_output.R")
source(file = "./rcode/sumsim/sum_processed_output.R")

# Select datagen_scenarios and analysis_scenarios to be used
use_datagen_scenarios <- datagen_scenarios()[42,]
use_analysis_scenarios <- analysis_scenarios()[analysis_scenarios()$size_valdata == 0.4,]

##############################
# 1 - Run simulation study 
##############################
# not run
# run_sim()
run_sim(rep = 2,
        use_datagen_scenarios = use_datagen_scenarios,
        use_analysis_scenarios = use_analysis_scenarios)

##############################
# 2 - Process simulation output 
##############################
# not run
# process_sim_output()
process_sim_output(use_datagen_scenarios = use_datagen_scenarios,
                   use_analysis_scenarios = use_analysis_scenarios)

##############################
# 3 - Summarize processed simulation output 
##############################
# not run
# summarise_sim()
summarise_sim(use_datagen_scenarios = use_datagen_scenarios,
              use_analysis_scenarios = use_analysis_scenarios,
              summary_file_name = "summary_diffme.Rds")
