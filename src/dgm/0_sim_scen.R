#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 0 - Simulation scenarios ----
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies ----
##############################

############################## 
# 1 - Helper Functions ----
##############################
# by increasing tau, we add more measurement error
# var_VAT = 2075, approximately the variance of VAT (estimated by generating a
# data set of size 1e7)
calc_tau <- function(R_squared, theta = 0.16, var_VAT = 2075){
  tau_squared <- ((1 - R_squared) * theta^2 * var_VAT) / R_squared
  return(sqrt(tau_squared))
}
# by changing lambda, we change the skewness of the data
calc_lambda <- function(skewness, k = 6.1){
  lambda <- 4 / (k * skewness^2)
  return(lambda)
}

############################## 
# 2 - Function that creates data.frame with 50 scenarios used to generate data
##############################
datagen_scenarios <- function(){
  skewness <- c(0.1, 1, 2, 4, 6)
  lambdas <- sapply(skewness, calc_lambda)
  R_squared <- c(0.2, 0.4, 0.6, 0.8, 0.9)
  taus <- sapply(R_squared, calc_tau)
  heteroscedastic <- c(TRUE, FALSE)
  
  # data.frame with simulation scenarios
  datagen_scenarios <- expand.grid(lambdas, 
                                   taus, 
                                   heteroscedastic)
  colnames(datagen_scenarios) <- c("lambda", 
                                   "tau", 
                                   "heteroscedastic")
  return(datagen_scenarios)}
############################## 
# 2 - Function that creates data.frame with 50 scenarios, differently analysing
# the data
##############################
analyse_scenarios <- function(){
  size_valdata <- c(0.10, 0.25, 0.40, 0.50)
  sampling_strat <- c("Random", "Uniform", "Extremes")
  method <- c("complete_case",
              "naive",
              "reg_cal",
              "efficient_reg_cal",
              "inadm_reg_cal")
  
  # data.frame with simulation scenarios
  analyse_scenarios <- expand.grid(size_valdata, 
                                   sampling_strat, 
                                   method)
  colnames(analyse_scenarios) <- c("size_valdata", 
                                   "sampling_strat", 
                                   "method")
  return(analyse_scenarios)}