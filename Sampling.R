# What height represents 50% or less of young men.
# Find a range of heights that contains 95% of young men.
margin_of_error = function(score, std, n){
  
  score*(std/sqrt(n))
  
}

attr(margin_of_error, "Margin of Error") <- "calculates margin of error, given a t or z score (score), standard deviation (std) and sample size (n)."

# What height represents 50% or less of young men.
# Find a range of heights that contains 95% of young men.
opt_sample_size = function(score, std, ME){
  
  (score^2*std^2)/ME^2
  
}

attr(opt_sample_size, "Optimal Sample Size") <- "calculates optimal sample size, given a t or z score (score), standard deviation (std) and margin of error (ME)"