#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 3 - regression calibration
## lindanab4@gmail.com - 20200303
#############################################################
reg_cal <- function(data){
  # using WC io VAT (introducing meas error)
  naive_fit <- naive(data)
  # fit calibration model
  cal_mod <- lm(VAT ~ WC + TBF + age + sex,
                data = data, 
                na.action = na.omit)
  sum_cal_mod <- summary(cal_mod)
  lambda <- sum_cal_mod$coefficients["WC", "Estimate"]
  var_lambda <- sum_cal_mod$coefficients["WC", "Std. Error"]
  # correct for measerror
  beta <- naive_fit["beta"] / lambda
  # estimate variance using delta method
  var_beta <- 1 / lambda^2 * naive_fit["var_beta"] + 
    naive_fit["beta"]^2 / lambda^4 * var_lambda
  # output is a vector with the corrected coef (beta) and its variance
  out <- c(beta = unname(beta), var_beta = unname(var_beta))
}
