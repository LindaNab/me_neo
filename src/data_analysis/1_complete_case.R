#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 1 - complete case analysis
## lindanab4@gmail.com - 20200303
#############################################################
complete_case <- function(data){
  fit <- lm(IR_ln ~ VAT + TBF + age + sex, 
            data = data, 
            na.action = na.omit)
  sum_fit <- summary(fit)
  beta <- sum_fit$coefficients["VAT", "Estimate"]
  var_beta <- sum_fit$coefficients["VAT", "Std. Error"]
  out <- c(beta = beta, var_beta = var_beta)
}