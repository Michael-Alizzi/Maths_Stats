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

# A call center receives calls at an average rate of 12 calls per hour. 
# Assuming the time between calls follows an exponential distribution, 
# what is the probability that the next call arrives at exactly 5 minutes?
exp_prob_k = function(B, x){
  
  exp_prob_k <- (1/B)*exp(-x)/B
  
  return(exp_prob_k)
  
}

attr(exp_prob_k, "Exponential Probability of k") <- "Assuming the random variable is a Exponential distribution, computes the time until the next event, given the average of a random variable (lambda or 1/B)."

# A call center receives calls at an average rate of 12 calls per hour. 
# Assuming the time between calls follows an exponential distribution, 
# what is the probability that the next call arrives within the next 5 minutes?
exp_prob_k_dist = function(B, x){
  
  # Create a data frame to store probability distribution
  exp_prob_dist_df <- data.frame(x = 0:x, 
                                   P = numeric(x + 1)
  )
  
  for(i in 0:x){
    
    exp_prob_k <- (1/B)*exp(-i)/B
    
    exp_prob_dist_df$P[i + 1] <- exp_prob_k
    
  }
  
  return(exp_prob_dist_df)
  
}

attr(exp_prob_k_dist, "Exponential Probability Distribution of k") <- "Assuming the random variable is a Exponential distribution, computes the up to any time until the next event, given the average of a random variable (lambda or 1/B)."

uni_density_funct = function(a, b){
  
uni_density <- 1/(b-a)
  
  return(uni_density)
}

attr(uni_density_funct, "Density Distribution of a continuous Random Variable with a Uniform Distribution") <- "The output and probability parameters must be numeric vectors."

uni_desc_funct = function(a, b){
  
  # Mean of a continuous random variable
  uni_rand_mean_value <-(a+b)/2
  
  # Variance of a continuous random variable
  uni_rand_var_value <- (a-b)^2/12
  
  # Standard deviation of a continuous random variable
  duni_rand_std_value <- sqrt(uni_rand_var_value)
  
  return(list(mean = uni_rand_mean_value, 
              Variance = uni_rand_var_value,
              StandardDeviation = duni_rand_std_value))
}

attr(uni_desc_funct, "Mean, Variance and Standard Deviation of a Continuous Random Variable with a Uniform Distribution") <- "The output and probability parameters must be numeric vectors."

# The distribution of young men's heights is approximately normal 
# with mean 174 cm and standard deviation 6.4 cm.
# 1. What percentage of these men are taller than six feet (182.9 cm)?
# 2. What's the chance that a randomly selected young man is 170-something cm tall?
normal_dist_prob = function(x, y = NA, avg, std, PX = 2){
  
  if(PX == 0) {
  # Equal to or less than x. P(X<=x)
  Prob_X <- pnorm(x, mean = avg, sd = std)
  
  } else if(PX == 1) {
  
  # Greater than x. P(X>x)
  Prob_X <- 1-pnorm(x, mean = avg, sd = std)
  
  } else {
  
  if(!is.na(y)) {
    
  # Less than y and equal to or greater than x. P(X<y) & P(X>=x)
    Prob_X <- pnorm(y, mean = avg, sd = std)-pnorm(x, mean = avg, sd = std)
  
  }
    
  }
  
  return(Prob_X)
  
}
attr(normal_dist_prob, "Chance of a Continuous Random Variable with a Normal Distribution") <- "Given the mean and standard deviation in a normal distribution, what is the chance of x, y or the difference between the two. When PX equals 0, it outputs the chance that X is equal to or less than x. When PX equals 1, then outputs the chance of X being greater than x. Leave PX empty and set a y parameter for the chance of X being Less than y and equal to or greater than x."

# What height represents 50% or less of young men.
# Find a range of heights that contains 95% of young men.
normal_dist_percent = function(p1, p2, avg, std, PX = NA){
  
  if(is.na(PX)) {

    x_percentile <- qnorm(p1, mean = avg, sd = std)
    
    return(x_percentile)
    
  } else {
    
    # Greater than x. P(X>x)
    p1_percentile <- qnorm(p1, mean = avg, sd = std)
    p2_percentile <- qnorm(p2, mean = avg, sd = std)
    
    return(list(x1 = p1_percentile, x2 =p2_percentile))
  }
    
}

attr(normal_dist_percent, "X at any Percentile Probability of a Continuous Random Variable with a Normal Distribution") <- "Given the mean and standard deviation in a normal distribution, what is the value of x at any probability. The probability represents that X is equal to or less than x. Set PX to 1 and any two percentiles (a value less than 1) to P1 and P2 to get a range of x"

