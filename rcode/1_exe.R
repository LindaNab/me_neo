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

# not run
# run_sim()

##############################
# 1 - Run simulation study 
##############################
run_sim(rep = 10, 
        use_datagen_scenarios = datagen_scenarios()[1:2,])

# if you ran the simulation study once, and want to run it again, remove 
# directories in ./data/output
dirs <- paste0("./data/output/size_valdata_", 
               unique(analysis_scenarios()[['size_valdata']]) * 100) 
for (i in 1:length(dirs)){
  if(dir.exists(dirs[i])){
    system(paste0("rm -r ", dirs[i]))
  }
}
