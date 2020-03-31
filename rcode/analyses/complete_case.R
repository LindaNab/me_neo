#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 1 - complete case analysis
## lindanab4@gmail.com - 20200303
#############################################################
complete_case <- function(data){
  # select subjects of whom VAT is measured (complete cases)
  data_cc <- subset(data, in_valdata == 1)
  # estimate effect in complete cases
  fit <- lm(IR_ln ~ VAT + TBF + age + sex, 
            data = data_cc)
  beta <- fit$coefficients
  vcov_beta <- vcov(fit)
  out <- list(beta = beta, vcov = vcov_beta)
}
