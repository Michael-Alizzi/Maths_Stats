# What is the entropy of a particular node?
entropy_funct <- function(pPlus, pMinus) {
  
  entropy <- ifelse(
    
    pPlus == 0,
    
    -pMinus * log2(pMinus),
    
    ifelse(
      
      pMinus == 0,
      
      -pPlus * log2(pPlus),
      
      -pPlus * log2(pPlus) - pMinus * log2(pMinus)
      
    )
    
  )
  
  return(entropy)
  
}


attr(entropy_funct, "Entropy Function") <- 
"Calculates entropy for a binary classification scenario. 
The parameter pPlus is the probability of a positive and 
pMinus is the probability of a negative."

# What is the entropy of outcome (sunny, outcast, raining)?
condition_entropy_funct <- function(pPlusXp, pMinusXp, Xp) {
  
  entropy_vals <- entropy_funct(pPlusXp, pMinusXp)
  
  weighted_entropy <- entropy_vals * Xp
  
  sum(weighted_entropy)
}

attr(condition_entropy_funct, "Conditional Entropy Function") <- 
"Calculates entropy for a binary classification scenario. 
The parameter pPlusXp is a vector of probabilities of a positive Y given a unique value of X.
The parameter pMinusXp is a vector of probabilities of a Negative Y given a unique value of X.
Xp is the probability of a the unique value X occuring over the whole sample."

# Generalized conditional entropy function
condition_entropy_data <- function(data, x_col, y_col, pos_label) {
  
  data %>%
    group_by(.data[[x_col]]) %>%
    summarise(
      n = n(),
      entropy = {
        y_dist <- table(.data[[y_col]]) / sum(table(.data[[y_col]]))

        entropy_funct(y_dist[[pos_label]], 1-y_dist[[pos_label]])
      },
      .groups = "drop"
    ) %>%
    mutate(weight = n / sum(n)) %>% 
    summarise(conditional_entropy = sum(weight * entropy)) %>%
    pull(conditional_entropy)
}

attr(condition_entropy_data, "Entropy Function") <- 
"Calculates entropy for a binary classification scenario. 
The parameter pPlus is the probability of a positive and 
pMinus is the probability of a negative."

# What is the entropy of a particular node given a split by an attribute?
information_gain_funct <- function(EntropyOverall, ConditionalEntropy) {
  
  InformationGain <- EntropyOverall-ConditionalEntropy
  
  return(InformationGain)
  
}

attr(information_gain_funct, "Information Gain Function") <- 
"Calculates information gain. 
The EntropyOverall parameter is the overall entropy of Y (Calculated by entropy_funct) 
and ConditionalEntropy is the entropy of Y given x (Calculated by condition_entropy_funct)."

