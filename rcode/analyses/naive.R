#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 2 - naive analysis
## lindanab4@gmail.com - 20200303
#############################################################
naive <- function(data){
  # using WC io VAT
  fit <- lm(IR_ln ~ WC + TBF + age + sex, 
            data = data, 
            na.action = na.omit)
  sum_fit <- summary(fit)
  beta <- sum_fit$coefficients["WC", "Estimate"]
  var_beta <- sum_fit$coefficients["WC", "Std. Error"]^2
  out <- c(beta = beta, var_beta = var_beta)
}