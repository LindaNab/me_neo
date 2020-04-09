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
  beta_and_vcov <- calc_beta_and_vcov(rc_fit, cc_fit)
  out <- list(beta = beta_and_vcov$beta, 
              vcov = beta_and_vcov$vcov_beta)
}

calc_beta_and_vcov <- function(rc_fit, cc_fit) {
  inv_vcov_rc_fit <- erc_solve(rc_fit$vcov)
  inv_vcov_cc_fit <- solve(cc_fit$vcov)
  if (all(is.na(inv_vcov_rc_fit))) {
    beta <- c(NA, NA, NA, NA, NA)
    vcov_beta <- matrix(NA, nrow = 5, ncol = 5)
  } else {
    beta <- solve(inv_vcov_rc_fit + inv_vcov_cc_fit) %*%
      (inv_vcov_rc_fit %*% rc_fit$beta +
         inv_vcov_cc_fit %*% cc_fit$beta)
    vcov_beta <- solve(inv_vcov_rc_fit + inv_vcov_cc_fit)
  }
  return(list(beta = beta, vcov_beta = vcov_beta))
}

erc_solve <- function(vcov) {
  out <- tryCatch(
    {
      solve(vcov)
    },
    error = function(cond){
      message(cond)
      return(matrix(NA, nrow = 5, ncol = 5))
    }
  )
  return(out)
} 
