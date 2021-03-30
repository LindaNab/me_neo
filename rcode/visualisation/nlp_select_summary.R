#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Select summary for visualisation of sim study
## lindanab4@gmail.com - 20201111
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file =  "./rcode/dgm/sim_scen.R")
library(data.table)

##############################
# 1 - Helper functions ----
##############################
# creates a list of summaries, for use_sampling_strats 
# using the selected use_size_valdata and use_method
select_summary <- function(summary, 
                           use_size_valdata, 
                           use_method, 
                           use_sampling_strats) {
  summary <- subset(
    summary,
    size_valdata == use_size_valdata &
      method == use_method &
      (R_squared == 0.2 | R_squared == 0.4 | R_squared == 0.6 | R_squared == 0.8) &
      (skewness == 0.1 | skewness == 1.5 | skewness == 3)
  )
  select_sampling_strat <- function(summary, use_sampling_strat){
    subset <- subset(summary,
                     sampling_strat == use_sampling_strat)
  }
  out <- lapply(as.list(use_sampling_strats),
                FUN = select_sampling_strat,
                summary = summary)
  names(out) <- use_sampling_strats
  out
}