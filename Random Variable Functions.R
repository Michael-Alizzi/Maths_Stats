# Probability that the first success (rolling a 6) happens after 4 failures (so occurs on the 5th roll).
geo_p_funct = function(failures, probability, multi){
  
  if(multi == 1){
    
    geo_p <- data.frame(X = 0:failures, 
                        P = numeric(failures + 1)
    )

    for(i in 0:failures) {
      #print(failures)
      
      P_single <- dgeom(i, prob=probability)

      geo_p$P[i+1] <- P_single
      
    }
    
  } else {
    
    geo_p_multi <- data.frame(X = 1, 
                              P = numeric(1)
    )
    
    P_single <- dgeom(failures, prob=probability)
    
    geo_p$P[failures+1] <- P_single
    
  }
  
  return(geo_p)
  
}
  

attr(geo_p_funct, "Geometric probability Function of a Discrete Random Variable") <- "If you want the full probability distribution then multi = 1. The failures parameter is how many failures until success and the probability parameter is the fixed proabbility of success."


# A charitable organization is running a rafﬂe as a fundraiser. 
# They offer a grand prize of $500, two second prizes of $100, and ten third prizes of $20 
# each. They plan to sell 1000 tickets at $2 per ticket. What is the average, variance and 
# standard deviation amount of money won with each ticket in the lottery?
disc_rand_funct = function(outcome, probability){
  
  # Mean of a discrete random variable
  disc_rand_mean_value <-sum(outcome*probability)
  
  # Variance of a discrete random variable
  disc_rand_var_value <- sum(((outcome-disc_rand_mean_value)^2)*probability)
  
  # Standard deviation of a discrete random variable
  disc_rand_std_value <- sqrt(disc_rand_var_value)
  
  return(list(mean = disc_rand_mean_value, 
              Variance = disc_rand_var_value,
              StandardDeviation = disc_rand_std_value))
}

attr(disc_rand_funct, "Mean, variance and standard deviation of a discrete Random Variable") <- "The output and probability parameters must be numeric vectors."

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

bin_rand_funct = function(n, p){
  
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

attr(disc_rand_funct, "Mean, variance and standard deviation of a binomial Random Variable") <- " Both the n (the number of trials) and p (probability of success) parameters must be numeric vectors of length 1."

std_from_mean = function(std, mean, x){
  (mean-x)/std
}

attr(disc_rand_funct, "How many standard deviations away from the mean is x") <- "std = Standard Deviation, mean, x = how many standard deviations away from the mean is x"

