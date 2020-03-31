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
            data = data)
  beta <- fit$coefficients
  vcov_beta <- vcov(fit)
  out <- list(beta = beta, vcov = vcov_beta)
}