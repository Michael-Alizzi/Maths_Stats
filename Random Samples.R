desc_stats_normal <- function(coefficients = NA, mean, variance, divisor = NA, n){
  if(all(is.na(coefficients)) & is.na(divisor)) {
    
    mean_dist <- mean*n
    
    var_dist <- variance*n
    
  } else if(!all(is.na(coefficients)) & is.na(divisor)){
    
    mean_dist <- sum(coefficients*rep(mean, n))
    
    var_dist <- (sum(coefficients^2*rep(variance, n)))
    
  } else if(!all(is.na(divisor))){
    
    mean_dist <- sum(rep(mean, n))/divisor
    
    var_dist <- (sum(rep(variance, n)))/(divisor^2)
  }
  
  
  return(list(average = mean_dist, variance = var_dist))
  
}

attr(desc_stats_normal, "Descriptive Statistics of multiple samples with a Normal Distribution") <- 
"Calculates the expected mean and variance of normally distributed random variables across multiple samples. 
The 'coefficients' parameter (if used) must be a numeric vector of length n. 
The 'mean' and 'variance' parameters must be numeric scalars. 
If 'divisor' is used (e.g., to compute the expected variance of a sample mean), it should typically be equal to n."

population_S2 <- function(x, var_X_bar, n, population = 0){
  if(population == 0) {
    
    S2 <- var(x)
    
  } else {
    
    S2 <- var_X_bar*n
    
  }
  
  return(S2)
  
}

VarStd_X_bar_funct <- function(E_S2, n){
  
  var_X_bar <- E_S2/n
  
  std_x_bar <- sqrt(var_X_bar)
  
  return(list(variance = var_X_bar, std = std_x_bar))
  
}

attr(VarStd_X_bar_funct, "Expected Variance and Standard Deviation of X Bar") <- 
  "Given the expected population variance (E_S2) and sample size (n), this function returns the variance and standard deviation of the sample mean."

attr(population_S2, "Calculates S2") <- 
"Input a numeric vector of length greater than 1 for the x parameter to get S2 (the variance of a sample). Set population parameter to 1, the var_X_bar 
parameter (variance of multiple samples or variance of X bar) and n (the number of samples) as numeric vectors of length 1 to get the expected value
of S2."

inv_linear_prob_trans_funct <- function(a, b, y_1, y_2, mean, sd, PX = 2){
  
  if(PX == 2) {
    
    Y_inverse <- linear_inverse_funct(a, b)
    
    y <- y_1
    
    x_1 <- eval(Y_inverse)
    
    y <- y_2
    
    x_2 <- eval(Y_inverse)
    
    pX <- normal_dist_prob(x = x_1, y = x_2, avg = mean, std = sd, PX = PX)
    
  } else {
    Y_inverse <- linear_inverse_funct(a, b)
    
    y <- y_1
    
    x_bar <- eval(Y_inverse)
    
    pX <- normal_dist_prob(x = x_bar, avg = mean, std = sd, PX = PX)
    
  }
  
  return(pX)
  
}

attr(inv_linear_prob_trans_funct, "Probability at any Transformed X") <- 
  "Inverses a linear funtion and calculates different probabilities. When PX equals 0, it outputs the chance that X_bar is equal to or less than x. When PX equals 
1, then outputs the chance of X being greater than x. Leave PX empty and set a y_1 and y_2 (higher number) parameter for the chance of X being Less than y and 
equal to or greater than x."

linear_x_trans_funct <- function(P1, a, b, mean, variance){
    
    sd <- sqrt(variance)
    
    x_bar <- normal_dist_percent(p1 = P1, avg = mean, std = sd)
    
    x <- a*x_bar+b
  
  return(x)
  
}

attr(linear_x_trans_funct, "Transformed X at any Probability") <- 
  "At any probability, what is the value of the transformed X bar."
