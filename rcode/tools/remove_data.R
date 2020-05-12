#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Remove output data
## lindanab4@gmail.com - 20200319
#############################################################
source("./rcode/tools/create_data_dirs.R")
source("./rcode/tools/file_handling.R")

# Remove the .Rds files of the used analysis_scenarios and datagen_scenarios
remove_files <- function(use_analysis_scenarios, 
                         use_datagen_scenarios,
                         data_dir = "./data/output"){
  invisible(apply(use_analysis_scenarios,
                  1,
                  FUN = remove_files_analysis_scenario,
                  use_datagen_scenarios = use_datagen_scenarios,
                  data_dir = data_dir))
}
# Removes selected data_dirs
remove_data_dirs <- function(use_analysis_scenarios, 
                             data_dir = "./data/output"){
  levels <- list(
    "size_valdata" = 
      unique(use_analysis_scenarios[['size_valdata']]) * 100,
    "method" = 
      unique(use_analysis_scenarios[['method']])
  )
  dirs <- list_data_dirs(data_dir = data_dir, levels = levels)
  for (j in length(dirs):1){ # start sub directories level
    for(i in 1:length(dirs[[j]])){ # remove all subdirectories
      if(dir.exists(dirs[[j]][i])){
        system(paste0("rmdir ", dirs[[j]][i]))
        print(paste0(dirs[[j]][i], " removed!"))
      }
      else print(dirs[[j]][i])
    }
  }
}
# Removes all files of one analysis_scenario
remove_files_analysis_scenario <- function(analysis_scenario,
                                           use_datagen_scenarios,
                                           data_dir){
  files <- seek_files_analysis_scenario(analysis_scenario,
                                        use_datagen_scenarios,
                                        data_dir)
  for (i in 1:length(files)){
    if (file.exists(files[i])){
      system(paste0("rm ", files[i]))
      print(paste0(files[i], " removed!"))
    }
  }
}
# Seeks files of one analysis_scenario
seek_files_analysis_scenario <- function(analysis_scenario,
                                         use_datagen_scenarios,
                                         data_dir){
  scen_nums <- use_datagen_scenarios[['scen_num']]
  apply(use_datagen_scenarios,
        1,
        FUN = seek_file,
        analysis_scenario = analysis_scenario,
        data_dir = data_dir)
}
