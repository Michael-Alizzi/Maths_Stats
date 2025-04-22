# Pull all functions
source('~/Maths_Stats/Packages.R')
source('~/Maths_Stats/Probability Functions.R')
source('~/Maths_Stats/Random Variable Functions.R')
source('~/Maths_Stats/Distribution Functions.R')
source('~/Maths_Stats/Sampling.R')
source('~/Maths_Stats/Transformations.R')
source('~/Maths_Stats/Random Samples.R')
source('~/Maths_Stats/Confidence Intervals and Hypothesis Tests.R')
source('~/Maths_Stats/Supervised and Unsupervised Machine Learning.R')
options(scipen = 999)

# Function metadata
attributes(t_test_p_stat_funct)

# Entropy question
TotalEntropy <- entropy_funct(3/7, 4/7)

fin <- 
  condition_entropy_funct(
    c(3/5, 0/2),
    c(2/5, 2/2),
    c(5/7, 2/7)
  )

water <- 
  condition_entropy_funct(
    c(0/3, 3/4),
    c(3/3, 1/4),
    c(3/7, 4/7)
  )

TotalEntropy-fin

TotalEntropy-water

WaterEntropy <- entropy_funct(0/3, 4/3)

fin2 <- 
  condition_entropy_funct(
    c(3/3, 0/1),
    c(0/3, 1/1),
    c(3/4, 1/4)
  )

df <- data.frame(
  x1 = c(0, 2, 1),
  x2 = c(1, 0, 1),
  y = c(-1, -1, 1)
)

# Neural network train question
perceptron_train(df, c(0, 2), -1.5, 1)

data <- c(8.74, 9.84, 10.10, 10.24, 10.68, 9.78, 10.74, 11.02, 
          10.44, 10.24, 10.84, 9.64, 10.36, 9.18, 10.28, 10.18, 
          9.74, 10.84, 8.24, 10.06, 10.44, 11.14, 10.28, 8.22) 

# hypothesis test question
stat_manual(mean(data), 10.4, sd(data), length(data))

t_test <- t_test_funct(data, 10.4, 0.01, 2)

t_stat <- t_test$t_stat

t_test_p_stat_funct(as.numeric(t_stat), length(data), tail = 2)
