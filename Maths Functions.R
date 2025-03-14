library('dplyr')

# Toss a 6-sided die 3 times.
# What is the number of outcomes in the sample space?
multi_rule <- function(outcomes) {
  
  return(prod(outcomes))  
  
}

attr(multi_rule, 'Multiplication Rule') <- "Calculates the product of all elements in a numeric vector. Used to determine total possible outcomes for k independent experiments, where each experiment has a specific number of outcomes. Input must be a numeric vector."

# If there are six different statistics textbooks (n = 6) on a 
# bookshelf, how many ways can you order the books (r = 6)?
perm_calc = function(n, r){
  
  return(factorial(n) / factorial(n - r))
  
  }

attr(perm_calc, 'Permutation Rule') <- 'Determines the total number of ways to arrange r objects selected from n distinct objects in a specific order.'

# A particular committee has four members (n = 4). One member must chair the committee, and 
# a different committee member must take minutes from meetings (r = 2). How many different 
# ways are there of choosing a Chair and a Minute-taker for this committee?
combination_calc = function(n, r){
  
  return(choose(n, r))

}

attr(combination_calc, 'Combination Rule') <- 'Calculates the number of ways to choose r objects from n distinct objects where order does not matter.'

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

attr(disc_rand_funct, 'Mean, variance and standard deviation of a discrete Random Variable') <- 'The output and probability parameters must be numeric vectors.'

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

attr(bin_prob_k, 'Binomial Probability of k') <- 'Computes the probability of getting exactly k successes in n events, given a success probability p.'

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

attr(disc_rand_funct, 'Mean, variance and standard deviation of a binomial Random Variable') <- ' Both the n (the number of trials) and p (probability of success) parameters must be numeric vectors of length 1.'

std_from_mean = function(std, mean, x){
  (mean-x)/std
}

attr(disc_rand_funct, 'How many standard deviations away from the mean is x') <- 'std = Standard Deviation, mean, x = how many standard deviations away from the mean is x'


