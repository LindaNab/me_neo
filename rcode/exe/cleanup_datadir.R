#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Delete dirs + .Rds files in .data/ [output/processed/summarised]
## lindanab4@gmail.com - 20200403
#############################################################

##############################
# 0 - Load librairies + source code 
##############################
source(file = "./rcode/dgm/sim_scen.R")
source(file = "./rcode/tools/remove_data.R")

# Select datagen_scenarios and analysis_scenarios to be removed
use_datagen_scenarios <- datagen_scenarios()[1:10,]
use_analysis_scenarios <- analysis_scenarios()

##############################
# 1 - Remove data
##############################
# Remove simulation output
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