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
  # efficient regression calibration
  beta <- ( 1 / (1 / rc_fit["var_beta"] + 1 / cc_fit["var_beta"]) ) *
            ( rc_fit["beta"] / rc_fit["var_beta"] + 
              cc_fit["beta"] / cc_fit["var_beta"] )
  var_beta <- ( 1 / (1 / rc_fit["var_beta"] + 1 / cc_fit["var_beta"]) )
  out <- c(beta = beta, var_beta = var_beta)
}