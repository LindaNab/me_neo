#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 5 - inadmissable regression calibration
## lindanab4@gmail.com - 20200303
#############################################################
inadm_reg_cal <- function(data){
  # select subjects of whom VAT is measured (complete cases)
  data_cc <- subset(data, in_valdata == 1)
  # fit calibration model in complete cases
  cal_mod <- lm(VAT ~ WC + TBF + age + sex,
                data = data_cc, 
                na.action = na.omit)
  VAT_fitted <- predict.lm(cal_mod, newdata = data)
  # make copy of data that will be imputed
  data_imp <- data
  # impute fitted values if VAT is missing
  data_imp$VAT[data$in_valdata == 1] <- VAT_fitted[data$in_valdata == 1]
  data_imp$in_valdata <- 1
  # perfomr complete case analysis on imputed data
  cc_fit <- complete_case(data_imp)
  out <- cc_fit
}