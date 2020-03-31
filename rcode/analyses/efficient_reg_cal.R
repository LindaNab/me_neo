#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 4 - efficient regression calibration
## lindanab4@gmail.com - 20200303
#############################################################
efficient_reg_cal <- function(data){
  # fit from complete case analysis
  cc_fit <- complete_case(data)
  # fit from regression calibration
  rc_fit <- reg_cal(data)
  # efficient regression calibration (Spiegelman, 2001)
  beta <- solve(solve(rc_fit$vcov) + solve(cc_fit$vcov)) %*%
    (solve(rc_fit$vcov) %*% rc_fit$beta + solve(cc_fit$vcov) %*% cc_fit$beta)
  vcov_beta <- solve(solve(rc_fit$vcov) + solve(cc_fit$vcov))
  out <- list(beta = beta, vcov = vcov_beta)
}