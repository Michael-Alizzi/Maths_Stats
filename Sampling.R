# Suppose we have a sample of n = 9,421 properties with sale price in $’000 (price)
# Sample mean of price is 1073.4 ($1.0734 million) with a sample standard deviation of 661.7
standard_error = function(var, n){
  
  SE <- sqrt(var/n)
  
  return(ME)
  
}

attr(standard_error, "Standard Error") <- 
"calculates standard error, given variance (var) and sample size (n)."

# Suppose we have a sample of n = 9,421 properties with sale price in $’000 (price)
# Sample mean of price is 1073.4 ($1.0734 million) with a sample standard deviation of 661.7
margin_of_error = function(score, std, n, SE = NA){
  
  if(is.na(SE)) {
    
  ME <- score*(std/sqrt(n))
  
  } else {
    
    ME <- score*SE
    
  }
  
  return(ME)
  
}

attr(margin_of_error, "Margin of Error") <- 
"calculates margin of error, given a t or z score (score), standard deviation (std) and sample size (n). Or standard error (SE) and a t or z score (score)"

# Suppose we have a sample of n = 9,421 properties with sale price in $’000 (price)
# Sample mean of price is 1073.4 ($1.0734 million) with a sample standard deviation of 661.7
opt_sample_size = function(score, std, ME){
  
  (score^2*std^2)/ME^2
  
}

attr(opt_sample_size, "Optimal Sample Size") <- 
"calculates optimal sample size, given a t or z score (score), standard deviation (std) and margin of error (ME)"


ceiling(opt_sample_size(score = 1.96,std = sqrt(0.5*(1-0.5)),ME = 0.03))

                