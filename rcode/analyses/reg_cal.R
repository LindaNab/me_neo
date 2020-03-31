#############################################################
## Internal validation sampling strategies for exposure 
## measurement error correction
##
## 3 - regression calibration
## lindanab4@gmail.com - 20200303
#############################################################

############################## 
# 1 - Regression Calibration 
##############################
reg_cal <- function(data){
  # using WC io VAT (introducing meas error)
  naive_fit <- naive(data)
  beta_star <- naive_fit$beta
  # fit calibration model
  # select subjects of whom VAT is measured (complete cases)
  cal_mod <- lm(VAT ~ WC + TBF + age + sex,
                data = subset(data, in_valdata == 1))
  beta <- correct_beta_star(beta_star, cal_mod)
  vcov_beta <- get_vcov_beta(beta_star, cal_mod)
  # output is a list with the corrected coef (beta) and its variance
  out <- list(beta = beta, vcov = vcov_beta)
}

############################## 
# 2 - Functions that correct for measurement error and get the vcov matrix
# of the corrected estimator using the Delta method
##############################
correct_beta_star <- function(beta_star, cal_mod){
  beta_star_names <- names(beta_star)
  beta_star <- change_order_coefs(beta_star)
  measerr_mat <- get_measerr_mat(cal_mod)
  beta <- beta_star %*% solve(measerr_mat)
  beta <- change_order_coefs(beta)
  names(beta) <- beta_star_names
  beta
}
# estimate vcov matrix using delta method
get_vcov_beta <- function(beta_star, cal_mod){
  # Step 2. covariance matrix of the corrected beta
  # take vec = (c(beta_star), c(cal_mat))
  # = (c(beta_star), lambda_1, 0, 0, ...)
  vec_measerr_mat <- c(get_measerr_mat(cal_mod))
  vec <- c(unname(change_order_coefs(beta_star)), 
           vec_cal_mat(vec_measerr_mat))
  # The covariance matrix of vec is of size 30 x 30
  # We assume that there is no covariance between beta_star and vec_cal_mat 
  # Thus, vcov(vec) = (vcov(beta_star), 0
  #                   0,               vcov(vec_cal_mat)) # see step 2
  vcov_vec_cal_mat <- get_vcov_vec_cal_mat(cal_mod)
  vcov_vec <- matrix(0, nrow = 30, ncol = 30)
  vcov_vec[1:5, 1:5] <- change_order_vcov(naive_fit$vcov)
  vcov_vec[6:30, 6:30] <- vcov_vec_cal_mat
  # We assume that vec is multivariate normal with mean vec and cov vcov_vec
  # Now, there is a function f: R^30 -> R^5 so that
  # f(vec) = beta 
  # Then, using the delta method vcov(beta) = Jf %*% vcov_vec %*% t(Jf)
  f <- function(vec, nb){
    # nb = number of elements in beta_star
    beta <- numeric(nb)
    for(i in 1:nb){
      beta[i] <- sum(vec[1:nb]*vec[(i*nb+1):((i+1)*nb)])
    }
    beta
  }
  jf <- numDeriv::jacobian(f, vec, nb = 5) 
  vcov_beta <- jf %*% vcov_vec %*% t(jf)
  vcov_beta <- change_order_vcov(vcov_beta)
  colnames(vcov_beta) <- names(beta_star)
  rownames(vcov_beta) <- names(beta_star)
  vcov_beta
}
get_vcov_vec_cal_mat <- function(cal_mod){
  # Step 1. vcov(vec_cal_mat): covariance matrix of the vectorised inverse
  # of the measerr_mat (formerly known as A = solve(Lambda))
  
  # measerr_mat = (lambda_1, lambda_0, lambda_2, ..
  #                0         1         0       , ..
  #                ..                              )
  measerr_mat <- get_measerr_mat(cal_mod)
  # vcov_cal_mod = (var(lambda_1), cov(lambda_1, lambda_0), ..
  #                 cov(lambda_0, lambda_1), var(lambda_0), ..
  #                 ..                                        )
  vcov_cal_mod <- get_vcov_cal_mod(cal_mod)
  # vec_measerr_mat is the vectorised measerr_mat
  # vec_measerr_mat = (lambda_1, 0, 0, 0, 0, lambda_0, 1, 0, 0, 0, ....)
  vec_measerr_mat <- c(measerr_mat)
  # the vcov matrix of the vectorised measerr_mat is known: 
  # it is a 25 x 25 matrix 
  # vcov(vec_measerr_mat) = (var(lambda_1), cov(lambda_1, 0), ..., 
  #                                         cov(lambda_1, lambda_0), .. (row#1)
  #                          cov(0, lambda_1), var(0), ...,
  #                                          cov(0, lambda_0), .. (row #2) .. 
  # Using the deltamethod, we use that vec_measerr_mat is multivariate normal
  # with mean (lambda_1, 0, 0, 0, 0, lambda_0, 1, 0, 0, 0, ....) and variance
  # vcov(vec_measerr_mat). There is a function g: R^25 -> R^25 so that 
  # g(vec_measerr_mat) = vec_cal_mat where vec_cal_mat is the vectorised 
  # calibration matrix (formerly known as A). Then, 
  # vcov(vec_cal_mat) = Jg %*% vcov(vec_measerr_mat) %*% t(Jg)
  vcov_vec_measerr_mat <- matrix(0, nrow = NROW(cal_mod$coefficients)^2, 
                                 ncol = NROW(cal_mod$coefficients)^2)
  for(i in 1:{n <- nrow(vcov_cal_mod)}){
    for(j in 1:n)
      vcov_vec_measerr_mat[(1 + n * (i - 1)),
                           (1 + n * (j - 1))] <- vcov_cal_mod[i, j]
  }
  # cal_mat is the calibration matrix, wich is the inverse of the measerr_mat
  # this function vectorizes the calibration matrix, using the vectorised 
  # c(measerr_mat) = (lambda_1, 0, 0, 0, 0, lambda_0, 1, 0, 0, 0, ....)
  # Jacobian of the vectorised calibration matrix
  J_vec_cal_mat <- numDeriv::jacobian(vec_cal_mat, vec_measerr_mat)
  # Thus, using the Delta method, vcov(vec_cal_mat) is: 
  vcov_vec_cal_mat <- J_vec_cal_mat %*% 
    vcov_vec_measerr_mat %*% t(J_vec_cal_mat)
  vcov_vec_cal_mat
}

############################## 
# 3 - Helper functions
##############################
# vectorised calibration matrix
vec_cal_mat <- function(vec_measerr_mat, n = sqrt(NROW(vec_measerr_mat))) {
  measerr_mat <- matrix(vec_measerr_mat, nrow = n)
  c(solve(measerr_mat))} # in notes function g
# Function that creates the measerror matrix Lambda, the inverse of Lambda
# solves the measurement error (= A the calibration matrix)
get_measerr_mat <- function(cal_mod){
  lambdas <- cal_mod$coefficients
  lambdas <- change_order_coefs(lambdas)
  # Create measurement error matrix
  measerr_mat <- diag(NROW(lambdas))
  measerr_mat[1,] <- lambdas
  colnames(measerr_mat) <- names(lambdas)
  rownames(measerr_mat) <- names(lambdas)
  measerr_mat
}
# Function that creates the vcov of the calibration model (i.e changes the
# order of the vcov of the cal_mod)
get_vcov_cal_mod <- function(cal_mod){
  vcov_mat <- vcov(cal_mod)
  vcov_mat <- change_order_vcov(m = vcov_mat)
  colnames(vcov_mat) <- change_order_coefs(colnames(vcov_mat))
  rownames(vcov_mat) <- change_order_coefs(rownames(vcov_mat))
  vcov_mat
}
# Function that exchanges second and first coef
change_order_coefs <- function(coefs){
  coefs <- c(coefs[2], coefs[1], coefs[3:5])
}
# Function that exchanges second and first entry of vcov matrix
change_order_vcov <- function(m){
  #save upper 2x2 matrix
  temp <- m[1:2, 1:2]
  #rearrange diagonal
  diag(temp) <- c(temp[2,2], temp[1,1])
  #ruil 1e rij met 2e rij
  m[1:2,] <- rbind(m[2,], m[1,])
  #ruil 1e kolom met 2e kolom
  m[,1:2] <- cbind(m[,2], m[,1])
  #Now replace upper 2x2 
  m[1:2,1:2] <- temp
  return(m)
}
