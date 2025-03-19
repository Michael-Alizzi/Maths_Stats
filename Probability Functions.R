# Toss a 6-sided die 3 times.
# What is the number of outcomes in the sample space?
multi_count_calc <- function(outcomes) {
  
  return(prod(outcomes))  
  
}

attr(multi_count_calc, "Multiplication Rule in Counting") <- "Calculates the product of all elements in a numeric vector. Used to determine total possible outcomes for k independent experiments, where each experiment has a specific number of outcomes. Input must be a numeric vector."

# A particular committee has four members (n = 4). One member must chair the committee, and 
# a different committee member must take minutes from meetings (r = 2). How many different 
# ways are there of choosing a Chair and a Minute-taker for this committee?
perm_calc = function(n, r){
  
  return(factorial(n) / factorial(n - r))
  
  }

attr(perm_calc, "Permutation Rule") <- "Determines the total number of ways to arrange r objects selected from n distinct objects in a specific order."

# From a committee of four people(n = 4), two committee members will need to 
# present the committee's recommendations to the board of directors (r = 2).
# How many ways are there of choosing two committee members to report to the 
# board of directors?
combination_calc = function(n, r){
  
  return(choose(n, r))

}

attr(combination_calc, "Combination Rule") <- "Calculates the number of ways to choose r objects from n distinct objects where order does not matter."

# If there was only two parties in the US Senate, 
# whats the probability of being a republican if 
# the probability of being a democrat is 0.53.
complement_rule <- function(A) {
  
  Ac = 1-A 
  
  return(Ac)
}

attr(complement_rule, "Complement Rule") <- "Calculates the complement of P(A). i.e. P(Ac)."

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

attr(disj_prob, "Disjoint Check & Probability") <- "Determines if P(A) and P(B) are disjoint (mutually exclusive) and calculates P(A or B)."

# The probability of being an athlete who
# also wants an Olympic gold medal if: 
# The probability of being an athlete is 0.37 P(A).
# The probability of wanting an Olympic gold medal given someone is an athlete is 0.73 P(B|A).
condit_prob <- function(A, AandB) {
  
  BgivenA <- AandB/A
  
  return(BgivenA)
}

attr(condit_prob, "Conditional Probability") <- "Calculates the probability of P(B|A) when A and B are NOT independent."

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
    
    cat("Use the multi_prob_calc function to get P(A & B) or the condit_prob function to get P(A|B). \n")
    
  }
    
  }
  
  if(!is.na(AgivenB) & !is.na(A)){
    
    if(AgivenB == A){
      
      cat("Yes \n")
      
      cat(sprintf("P(A|B): %s \n", A))
      
    } else {
      
      cat("No \n")
      
      cat("Use the multi_prob_calc function to get P(A & B) or the condit_prob function to get P(A|B) or P(B|A). \n")
      
    }
    
  }
  
}

attr(pair_indep_prob, "Pairwise Independence Check & Probability") <- "Determines whether P(A) and P(B) are independent using P(A & B) = P(A)P(B) or P(A|B) = P(A). Provide either: A, B, and AandB (to check P(A & B) = P(A)P(B)) OR A and AgivenB (to check P(A|B) = P(A))."

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
    
    cat(sprintf("Probability of all event's intersection: %s \n", expected_prob))
    
  } else {
    
    cat("No \n")
    
    cat(sprintf("Probability of all event's intersection: %s \n", multi_event_prob))
    
  }
  
  
}

attr(mutu_indep_prob, 'Mutual Independence Check & Probability') <- "Determines whether any number of events are mutually independent. Include each event's individual probabilities as a numeric vector of length greater than 2 in the single_event_prob_vec parameter and the probability intersection of all events as a numeric vector of length 1 in the multi_event_prob parameter"

# The probability of being an athlete who
# also wants an Olympic gold medal P(B) if: 
# The probability of being an athlete is 0.37 P(A).
# The probability of wanting an Olympic gold medal given someone is an athlete is 0.73 P(B|A).
multi_prob_calc <- function(A, BgivenA) {
  
  AandB <- A*BgivenA 
  
  return(AandB)
}

attr(multi_prob_calc, 'Multiplication Rule in Probability with two events') <- "Calculates the probability of P(A & B) when A and B are NOT independent."

# The probability of being an athlete who
# also wants an Olympic gold medal P(B) if: 
# The probability of being an athlete is 0.37 P(A).
# The probability of wanting an Olympic gold medal given someone is an athlete is 0.73 P(B|A).
multi_event_prob_calc <- function(CgivenAandB, BgivenA, A) {
  
  AandBandC <- CgivenAandB*BgivenA*A
  
  return(AandBandC)
}

attr(multi_event_prob_calc, 'Multiplication Rule in Probability with More Than Two Events') <- "Calculates the probability of any number of events occuring together when the events are NOT independent. The CgivenAandB P(C|A&B) parameter is the probability that C occurs given A and B have already occurred. The BgivenA P(B|A) parameter is the probability that B occurs given A has already occured. The A P(A) parameter is the probability that A occurs"

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

# Urn I contains 3 red balls and 4 white balls.
# Urn II contains 2 red balls and 4 white balls.  
# A ball is drawn from Urn I and placed unseen into Urn II.
# A ball is now drawn at random from Urn II.
# What is the probability that this second ball is red P(B)?
law_of_prob = function(BgivenAi, Ai){
  
  B <- sum(BgivenAi*Ai)
  
  return(B)
}

attr(law_of_prob, "The Law of Total Probability") <- "Computes P(B) using the Law of Total Probability, given P(B|Ai) and P(Ai). The BgivenAi parameter must contain P(B|Ai) as a numeric vector, and the Ai parameter must contain the probabilities of P(Ai) as a numeric vector."

# Urn I contains 3 red balls and 4 white balls.
# Urn II contains 2 red balls and 4 white balls.  
# A ball is drawn from Urn I and placed unseen into Urn II.
# A ball is now drawn at random from Urn II.
# Given that the second ball drawn is red P(B), what is the probability that the first ball was white P(Aj|B)?
bayes_funct = function(BgivenAj, Aj, B){
  
  AjgivenB <- (BgivenAj*Aj)/B
  
  return(AjgivenB)
}

attr(bayes_funct, "Bayes' Theorem") <- "Computes P(Aj|B) using Bayes' Theorem, given P(B|Aj), P(Aj), and P(B). The BgivenAj parameter must contain the probability of B given Aj (P(B|Aj)) as a numeric vector, Aj must contain the probability of Aj as a numeric vector, and B must be the probability of B as a numeric value."
