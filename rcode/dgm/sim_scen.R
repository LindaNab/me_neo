#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## Simulation scenarios
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies ----
##############################
source(file = "./rcode/dgm/gen_data.R")

############################## 
# 1 - Helper Functions ----
##############################
# by increasing tau, we add more measurement error
# var_VAT = 2075, approximately the variance of VAT (estimated by generating a
# data set of size 1e7)
calc_tau <- function(R_squared, theta = 0.8){
  # var_VAT <- calc_var_VAT()
  var_VAT <- 1.219695 # hard-coded to make code more efficient
  tau_squared <- ((1 - R_squared) * theta^2 * var_VAT) / R_squared
  return(sqrt(tau_squared))
}
calc_var_VAT <- function(){
  data <- gen_data(nobs = 1e7,
                   lambda = 1, 
                   theta = 1,
                   tau = 0,
                   seed = 20200331)
  var(data$VAT)
}
# by changing lambda, we change the skewness of the data
calc_lambda <- function(skewness, k = 6.1){
  lambda <- 4 / (k * skewness^2)
  return(lambda)
}

############################## 
# 2 - Function that creates data.frame with 50 scenarios used to generate data
#     varying lambda, tau and whether the measurement error model is linear or 
#     non-linear (1 or 0, respectively)
##############################
datagen_scenarios <- function(){
  skewness <- c(0.1, 1, 1.5, 3)
  lambdas <- sapply(skewness, calc_lambda)
  R_squared <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  taus <- sapply(R_squared, calc_tau)
  linear <- c(1, 0) #(yes, no)
  
  # data.frame with simulation scenarios
  datagen_scenarios <- expand.grid(lambdas, 
                                   taus, 
                                   linear)
  # Add theta (theta = 0.16 in scenarios 1-50 but theta = 1 in scenario 0)
  datagen_scenarios$theta <- 0.8
  # Add scenarios nums
  datagen_scenarios$scen_num <- c(1:NROW(datagen_scenarios))
  colnames(datagen_scenarios) <- c("lambda", 
                                   "tau", 
                                   "linear",
                                   "theta",
                                   "scen_num")
  # add corresponding R_squared to taus (convenient for results)
  R_squared_tau_pairs <- cbind(R_squared, taus)
  datagen_scenarios <- merge(datagen_scenarios, R_squared_tau_pairs, 
                             by.x = 'tau', by.y = 'taus')
  # add corresponding skewness to lambdas (convenient for results)
  skew_lambda_pairs <- cbind(skewness, lambdas)
  datagen_scenarios <- merge(datagen_scenarios, skew_lambda_pairs,
                             by.x = 'lambda', by.y = 'lambdas')
  # Use orderning according to scen_num
  datagen_scenarios <- datagen_scenarios[order(datagen_scenarios$scen_num),]
  # Add S0
  datagen_scenarios <- rbind(datagen_scenarios_S0(),
                             datagen_scenarios)
  return(datagen_scenarios)}

datagen_scenarios_S0 <- function(){
  R_squared = 1
  skewness = 0.1
  S0 <- c(lambda = calc_lambda(skewness),
          tau = calc_tau(R_squared, theta = 1),
          linear = 0,
          theta = 1,
          scen_num = 0,
          R_squared = R_squared,
          skewness = skewness)
}
############################## 
# 3 - Function that creates data.frame with 50 scenarios, differently analysing
# the data 
############################## 
analysis_scenarios <- function(){
  sampling_strat <- c("random", "uniform", "extremes")
  method <- c("complete_case",
              "naive",
              "reg_cal",
              "efficient_reg_cal",
              "inadm_reg_cal")
  size_valdata <- c(0.10, 0.25, 0.40, 0.50)
  size_valdata_percent <- 100 * size_valdata
  # data.frame with simulation scenarios
  analyse_scenarios <- expand.grid(sampling_strat,
                                   method,
                                   size_valdata)
  colnames(analyse_scenarios) <- c("sampling_strat",
                                   "method",
                                   "size_valdata")
  return(analyse_scenarios)}

############################## 
# 4 - Script that checks R-squared
############################## 
# R-squared is correct for S0-S20. If measurement error is non-linear, tau is 
# equal in those cases, but R_squared will be lower than desired (S21-S40)
# datagen_scenario <- datagen_scenarios()[21,]
# seed <- 20200330
# # from run_sim
# data <- gen_data(nobs <- 1e6, 
#                  lambda = datagen_scenario[['lambda']],
#                  theta = datagen_scenario[['theta']],
#                  tau = datagen_scenario[['tau']],
#                  linear = datagen_scenario[['linear']],
#                  seed = seed)
# # r_squared
# fit <- lm(WC ~ VAT, data = data)
# summary(fit)
