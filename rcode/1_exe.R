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
source(file = "./rcode/sim/process_sim_output.R")
source(file = "./rcode/sim/sum_processed_output.R")
source(file = "./rcode/tools/remove_data.R")

##############################
# 1 - Run simulation study 
##############################
# Select datagen_scenarios and analysis_scenarios to be used
use_datagen_scenarios <- datagen_scenarios()[c(1),]
use_analysis_scenarios <- analysis_scenarios()

# not run
# run_sim()
run_sim(rep = 10, 
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
# summarize_sim()
summarize_sim(use_datagen_scenarios = use_datagen_scenarios,
              use_analysis_scenarios = use_analysis_scenarios)


# Remove output and directories after test running the simulation
remove_files(use_analysis_scenarios,
             use_datagen_scenarios,
             data_dir = "./data/output")
remove_data_dirs(use_analysis_scenarios,
                 data_dir = "./data/output")
# Remove processed output and directories
remove_files(use_analysis_scenarios,
             use_datagen_scenarios,
             data_dir = "./data/processed")
remove_data_dirs(use_analysis_scenarios,
                 data_dir = "./data/processed")
# Remove summary
summarised_dir <- "./data/summarised"
file_name_summary <- paste0(summarised_dir, "/summary.Rds")
if (file.exists(file_name_summary)){
  system(paste0("rm ", file_name_summary))
  print(paste0(file_name_summary, " removed!"))
}

