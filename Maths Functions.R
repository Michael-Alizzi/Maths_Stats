# Toss a 6-sided die 3 times.
# What is the number of outcomes in the sample space?
sample_space_calc <- function(outcomes) {
  
  return(prod(outcomes))  
  
}

attr(sample_space_calc, 'Multiplication Rule') <- "Calculates the product of all elements in a numeric vector. Used to determine total possible outcomes for k independent experiments, where each experiment has a specific number of outcomes. Input must be a numeric vector."

# A particular committee has four members (n = 4). One member must chair the committee, and 
# a different committee member must take minutes from meetings (r = 2). How many different 
# ways are there of choosing a Chair and a Minute-taker for this committee?
perm_calc = function(n, r){
  
  return(factorial(n) / factorial(n - r))
  
  }

attr(perm_calc, 'Permutation Rule') <- 'Determines the total number of ways to arrange r objects selected from n distinct objects in a specific order.'

# From a committee of four people(n = 4), two committee members will need to 
# present the committee's recommendations to the board of directors (r = 2).
# How many ways are there of choosing two committee members to report to the 
# board of directors?
combination_calc = function(n, r){
  
  return(choose(n, r))

}

attr(combination_calc, 'Combination Rule') <- 'Calculates the number of ways to choose r objects from n distinct objects where order does not matter.'

# If there was only two parties in the US Senate, 
# whats the probability of being a republican if 
# the probability of being a democrat is 0.53.
complement_rule <- function(A) {
  
  Ac = 1-A 
  
  return(Ac)
}

attr(complement_rule, 'Complement Rule') <- "Calculates the complement of P(A). i.e. P(Ac)."

# The probability of being a republican or
# female if: 
# The probability of being female is 0.17 .
# The probability of being a republican is 0.47.
# The probability of both is 0.04.
add_rule <- function(A, B, AandB) {
  
  AorB <- A+B-AandB 
  
  return(AorB)
}

attr(add_rule, 'Additive Rule') <- "Calculates the probability of P(A or B) when A and B are NOT disjoint."

# The probability of pulling a jack and a
# seven from a standard deck of 52 cards.
disj_prob <- function(A, B, AorB) {
  
  cat("Are A and B disjoint? \n")
  
  if(AorB == A+B){
    
    cat("Yes \n")
    
    cat(sprintf("P(A or B): %s \n", A+B))
    
  } else {
    
    cat("No \n")
    
    cat("Use the add_rule function\n")
    
  }
  
}

attr(disj_prob, 'Disjoint Check & Probability') <- "Determines if P(A) and P(B) are disjoint (mutually exclusive) and calculates P(A or B)."

# The probability of being an athlete who
# also wants an Olympic gold medal P(B) if: 
# The probability of being an athlete is 0.37 P(A).
# The probability of wanting an Olympic gold medal given someone is an athlete is 0.73 P(B|A).
multi_rule <- function(A, BgivenA) {
  
  AandB <- A*BgivenA 
  
  return(AandB)
}

attr(multi_rule, 'Multiplicative Rule') <- "Calculates the probability of P(A & B) when A and B are NOT independent."

# The probability of being an athlete who
# also wants an Olympic gold medal if: 
# The probability of being an athlete is 0.37 P(A).
# The probability of wanting an Olympic gold medal given someone is an athlete is 0.73 P(B|A).
condit_prob <- function(A, AandB) {
  
  BgivenA <- AandB/A
  
  return(BgivenA)
}

attr(condit_prob, 'Conditional Probability') <- "Calculates the probability of P(B|A) when A and B are NOT independent."

# The probability of rolling a four on a six sided die 
# and rolling two dice and the sum being 7.
pair_indep_prob <- function(A = NA, B = NA, AandB = NA, AgivenB = NA) {
  
  cat("Are A and B pairwise independent? \n")
  
  if(!is.na(AandB) & !is.na(A) & !is.na(B)){
    
  if(AandB == A*B){
    
    cat("Yes \n")
    
    cat(sprintf("P(A & B): %s \n", A*B))
    
  } else {
    
    cat("No \n")
    
    cat("Use the multi_rule function to get P(A & B) or the condit_prob function to get P(A|B). \n")
    
  }
    
  }
  
  if(!is.na(AgivenB) & !is.na(A)){
    
    if(AgivenB == A){
      
      cat("Yes \n")
      
      cat(sprintf("P(A|B): %s \n", A))
      
    } else {
      
      cat("No \n")
      
      cat("Use the multi_rule function to get P(A & B) or the condit_prob function to get P(A|B) or P(B|A). \n")
      
    }
    
  }
  
}

attr(pair_indep_prob, 'Pairwise Independence Check & Probability') <- "Determines whether P(A) and P(B) are independent using P(A & B) = P(A)P(B) or P(A|B) = P(A). Provide either: A, B, and AandB (to check P(A & B) = P(A)P(B)) OR A and AgivenB (to check P(A|B) = P(A))."

# A coin is tossed twice.
# A be the event 'head on the first toss' .
# B the event 'head on the second toss'.
# C the event 'exactly one head turned up'.
#Show that A,B and C are pairwise independent but
# A,B and C are not independent.
mutu_indep_prob <- function(single_event_prob_vec, multi_event_prob) {
  
  expected_prob <- prod(single_event_prob_vec)
  
  cat("Are n events mutually independent? \n")
  
  if(isTRUE(all.equal(multi_event_prob, expected_prob))){
    
    cat("Yes \n")
    
    cat(sprintf("Probability of all events intersection: %s \n", expected_prob))
    
  } else {
    
    cat("No \n")
    
    cat(sprintf("Probability of all events intersection: %s \n", multi_event_prob))
    
  }
  
  
}

attr(mutu_indep_prob, 'Mutual Independence Check & Probability') <- "Determines whether any number of events are mutually independent. Include each event's individual probabilities as a numeric vector of length greater than 2 in the single_event_prob_vec parameter and the probability intersection of all events as a numeric vector of length 1 in the multi_event_prob parameter"

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

