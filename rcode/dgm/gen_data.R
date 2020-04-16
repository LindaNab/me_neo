#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 1 - Data Generation ----
## lindanab4@gmail.com - 20200221
#############################################################

##############################
# 0 - Load librairies ----
##############################

############################## 
# 1 - Generate data ----
##############################
gen_data <- function(nobs = 650, 
                     k = 6, 
                     lambda, 
                     theta, 
                     tau, 
                     beta = 0.2, 
                     sigma = sqrt(0.3),
                     linear = 1,
                     seed){
  # Initialize out_df that will be filled with the generated data
  out_df <- data.frame(sex = numeric(nobs), 
                       age = numeric(nobs), 
                       TBF = numeric(nobs), # total body fat
                       VAT = numeric(nobs), # visceral adipose tissue
                       WC = numeric(nobs), # waist circumference
                       IR_ln = numeric(nobs)) # insulin resistance
  # Function that calculates the mean of VAT
  calc_mean <- function(sex, age, TBF){
    0.4 - 2 * sex + 0.01 * age + 0.9 * TBF
  }
  set.seed(seed)
  # Generate sex, age and TBF, VAT, WC and IR_ln (natural log of IR)
  out_df$sex <- rbinom(n = nobs, size = 1, prob = 0.5)
  out_df$age <- runif(n = nobs, min = 45, max = 65)
  out_df$TBF <- with (out_df, rnorm(n = nobs, 
                                    mean = - 2 + 1 * sex + 0.01 * age,
                                    sd = sqrt(0.5)))
  shape <- lambda * k
  scale <- sqrt(0.5 / (lambda * k))
  out_df$VAT <- with (out_df, calc_mean(sex, age, TBF) - shape * scale + 
                        rgamma(n = nobs, 
                               shape = shape,
                               scale = scale))
  if (linear == 0){ # make linear 2 if measurement error is supposed to be 
    linear <- 2     # non-linear (linear: ^ 1 / 1; non-linear: ^ 1 / 2)
  }
  out_df$WC <- with (out_df, rnorm(n = nobs,
                                   mean = theta * sign(VAT) * 
                                     abs(VAT)^(1 / linear),
                                   sd = tau))
  out_df$IR_ln  <- with (out_df, rnorm(n = nobs,
                                       mean = 0.5 + beta * VAT - 0.5 * sex +
                                         0.01 * age + 0.3 * TBF,
                                       sd = sigma))
  out_df
}