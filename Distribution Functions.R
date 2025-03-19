linear_transform_normal <- function(coefficients, means, variances, divisor){
  mean_dist <- sum(coefficients*means)/divisor
  
  var_dist <- (sum(coefficients^2*variances))/(divisor^2)
  
  return(list(average = mean_dist, variance = var_dist))
}

attr(linear_transform_normal, "Linear Transformation of a Normal Distribution") <- "Determines the average and variance of a normally distributed random variable. The coefficients parameter must be a numeric vector with length greater than 1. And the variances and means parameters must be a numeric vector with a length of 1"

Bin_disc_rand_funct = function(n, p){
  
  # Mean of a binomial random variable
  disc_rand_mean_value <-n*p
  
  # Variance of a binomial random variable
  disc_rand_var_value <- n*p*(1-p)
  
  # Standard deviation of a binomial random variable
  disc_rand_std_value <- sqrt(disc_rand_var_value)
  
  return(list(mean = disc_rand_mean_value, 
              Variance = disc_rand_var_value,
              StandardDeviation = disc_rand_std_value))
}

attr(Bin_disc_rand_funct, "Mean, variance and standard deviation of a binomial Random Variable") <- " Both the n (the number of trials) and p (probability of success) parameters must be numeric vectors of length 1."

# A gambler places 4 bets (n = 4) on black in roulette, 
# where the probability of winning a single bet is 18/37 (p).
# What is the probability of winning 3 times (k) in
# any order?
bin_prob_k = function(n, p, k){
  
  # The binomial coefﬁcient gives the number of 
  # possible ways to arrange k successes in n events
  bin_co <- combination_calc(n, k)  
  
  k_prob <- bin_co * (p^k) * ((1 - p)^(n - k))
  
  return(k_prob)
  
}

attr(bin_prob_k, "Binomial Probability of k") <- "Computes the probability of getting exactly k successes in n events, given a success probability p."

# A gambler places 4 bets (n = 4) on black in roulette, 
# where the probability of winning a single bet is 18/37 (p).
# What is the probability of winning 1,2,3 or 4 times in
# any order?
bin_prob_dist = function(n, p){
  
  # Create a data frame to store probability distribution
  bin_prob_dist_df <- data.frame(X = 0:n, 
                                 P = numeric(n + 1)
  )
  # The number of different success outcomes 
  for (i in 0:n) {
    
    # The binomial coefﬁcient gives the number of 
    # possible ways to arrange k successes in n events
    # The combination rule is used because the order of wins and losses 
    # does not matter, we only care about the total number of wins.
    bin_co <- combination_calc(n, i)  
    
    # If a random variable X is binomial with n trials and 
    # probability of success p, the probability of getting exactly 
    # k successes (k in this case has been denoted as i in the loop)
    k_prob <- bin_co * (p^i) * ((1 - p)^(n - i))
    
    # Add the probability of a specific k number of successes to the created data frame
    bin_prob_dist_df$P[i + 1] <- k_prob
    
  }
  
  return(bin_prob_dist_df)
  
}

+(exp(1)^-5)*(5^0)/factorial(0)

# If, on average, 5 servers go offline during the day (lambda), 
# what is the chance that 1 will go offline (x)? 
# (Assume independence of servers going offline).
poisson_prob_k = function(lambda, x){
  
  poisson_k_prob <- (exp(-lambda))*(lambda^x)/factorial(x)
  
  return(poisson_k_prob)
  
}

attr(poisson_prob_k, "Poisson Probability of k") <- "Assuming the random variable is a Poisson distribution, computes the probability of getting exactly k counts, given the average of a random variable (lambda)."


# If, on average, 5 servers go offline during the day (lambda), 
# what is the chance less than 3 will go offline (x)? 
# (Assume independence of servers going offline).
poisson_prob_k_dist = function(lambda, x){
  
  # Create a data frame to store probability distribution
  poiss_prob_dist_df <- data.frame(x = 0:x, 
                                   P = numeric(x + 1)
  )
  
  for(i in 0:x){

  poisson_k_prob <- (exp(-lambda))*(lambda^i)/factorial(i)
  
  poiss_prob_dist_df$P[i + 1] <- poisson_k_prob
  
  }
  
  return(poiss_prob_dist_df)
  
}

attr(poisson_prob_k_dist, "Poisson Probability Distribution of k") <- "Assuming the random variable is a Poisson distribution, computes the probability of getting up to k number of counts, given the average of a random variable (lambda)."
