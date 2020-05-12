# this is visualisation code that is used to check the sampling strategies

source(file = "./rcode/sim/run_sim.R")
source(file = "./rcode/analyses/sample_valdata.R")
source(file = "./rcode/visualisation/scatterhistogram.R")
use_datagen_scenarios <- datagen_scenarios()
datagen_scenario <- use_datagen_scenarios[18,]
seed <- 20200331
# from run_sim
data <- gen_data(nobs = 650,
                 lambda = datagen_scenario[['lambda']],
                 theta = datagen_scenario[['theta']],
                 tau = datagen_scenario[['tau']],
                 linear = datagen_scenario[['linear']],
                 seed = seed)
# sample val_data
data <- select_valdata(data = data, 
                       size_valdata = 0.4, 
                       use_variable = "WC",
                       sampling_strat = "uniform",
                       seed = 20200512)
# make bins for uniform plot (copied code from analyses/sample_valdata.R )
# total number of subjects in data
n <- NROW(data)
# desired  number of subjects in valdata
n_valdata <- ceiling(n * 0.4)
n_bins <- 10
n_each_bin <- round(n_valdata / n_bins) # possibly less people included in
# validation sample due to rounding
bins <-
  create_bins(n_bins, 
              n_each_bin, 
              data, 
              use_variable = "WC", 
              n_valdata)
create_scatterhist(data, uniform = F, bins = bins)