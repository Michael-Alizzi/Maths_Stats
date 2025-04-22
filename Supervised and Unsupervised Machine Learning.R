# What is the entropy of  the overall sample?
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

# Generalized conditional entropy function when using actual data
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

# Generalized conditional entropy function when using actual data
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

# Using Bayes rule, given new Outlook, temperature, humidity and wind values, will they play tennis or not?
Bayes_prediction <- function(Total, TotalPositive, TotalNegative, PosVec, NegVec) {
  
  PositiveNumerator <- prod(PosVec/TotalPositive)*TotalPositive/Total
  
  NegativeNumerator <- prod(NegVec/TotalNegative)*TotalNegative/Total

  if(PositiveNumerator>NegativeNumerator) {
    
    cat("The prediction is positive")
    
  } else { 
    
    cat("The prediction is negative")
    
    }
  
}

attr(Bayes_prediction, "Prediction using Bayes Rule") <- 
  "Using Bayes rule, predicts binary outcome for new data based on existing data. 
The Total parameter is the total current rows. 
The TotalPositive parameter is the total current positive outcomes. 
The TotalNegative parameter is the total current negative outcomes.
The PosVec parameter are counts of features (as a vector) given a positive outcome.
The NegVec parameter are counts of features (as a vector) given a negative outcome."

perceptron_train <- function(df, w, b, rate) {
  for(i in 1:nrow(df)) {
    
    x <- as.numeric(df[i, 1:length(w)])
    
    y <- df$y[i]
    
    pred_val <- sum(w * x) + b
    
    pred_label <- ifelse(pred_val > 0, 1, -1)
    
    if(pred_label != y) {
      
      b <- b + rate * y
      
      w <- w + rate * y * x
      
    }
    
    cat("\nStep", i)
    
    cat("\nBias:\n", b)
    
    cat("\nWeights:\n")
    
    print(w)
    
    cat("---------------\n")
    
  }
  
  return(df)
  
}

attr(perceptron_train, "Training a Perceptron
     
     ") <- 
  ""

df <- data.frame(
  x1 = c(0, 2, 1),
  x2 = c(1, 0, 1),
  y = c(-1, -1, 1)
)

perceptron_train(df, c(0, 2), -1.5, 0.1)

