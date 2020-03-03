#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 5 - inadmissable regression calibration
## lindanab4@gmail.com - 20200303
#############################################################
inadm_reg_cal <- function(data){
  cal_mod <- lm(VAT ~ WC + TBF + age + sex,
                data = data, 
                na.action = na.omit)
  VAT_fitted <- fitted.values(cal_mod)
  # make copy of data that will be imputed
  data_imp <- data
  # impute fitted values if VAT is missing
  data_imp$VAT[is.na(data$VAT)] <- VAT_fitted[is.na(data$VAT)]
  # perfomr complete case analysis on imputed data
  cc_fit <- complete_case(data_imp)
  beta <- cc_fit["beta"]
  var_beta <- cc_fit["var_beta"]
  out <- c(beta = unname(beta), var_beta = unname(var_beta))
}