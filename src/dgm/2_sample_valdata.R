#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 1 - Helper functions
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies + source code 
##############################

############################## 
# 1 - Helper Functions ----
##############################
select_valdata <- function(data, 
                           use_variable = NA,
                           size_valdata,
                           sampling_strat){
  nobs <- NROW(data)
  number_in_valdata <- ceiling(n * size_valdata)
  if (sampling_strat == "random"){
    data$valdata_present <- sample(c(rep(0, {n - number_in_valdata}), 
                                    rep(1, number_in_valdata)),
                                    size = n,
                                    replace = FALSE)}
  if (sampling_strat == "uniform" & !is.na(use_variable)){
    number_in_sub_valdata <- round(number_in_valdata / 10)
    data$valdata_present <- rep(0, n)
    n_subjects <- numeric(10)
    probs <- quantile(unlist(data[use_variable]),
             probs = seq(0, 1, length.out = 11))
    
    for (i in 2:11){
      condition <- data[use_variable] < probs[i] & 
                     data[use_variable] >= probs[i-1] 
      n_subjects[i-1] <- NROW(data[condition,])
    }
    
    for (i in 2:11){
      condition <- data[use_variable] < probs[i] & 
                     data[use_variable] >= probs[i-1]
      data[condition,]$valdata_present <- c(rep(0, 
                                                {n_subjects[i-1] - 
                                                    number_in_sub_valdata}),
                                            rep(1, number_in_sub_valdata))
    }
    hist(data[data$valdata_present == 1,]$WC)
  }
  if (sampling_strat == "extreme" & !is.na(use_variable)){
    rownumbers <- c(order(data[use_variable])
                    [1:(number_in_valdata/2)],
                    order(data[use_variable])
                    [(n - number_in_valdata / 2 + 1):n])
    data$valdata_present <- rep(0, n)
    data[rownumbers,]$valdata_present <- 1
  }
  return(data)
}
