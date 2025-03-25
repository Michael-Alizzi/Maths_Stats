##------------------------------------------------------------------------------##

# Example. The distribution of young men's heights is approximately normal with 
# mean 174 cm and standard deviation 6.4 cm.

# Height X ~ N(174, 6.4^2), and Z = (X - 174)/6.4 ~ N(0,1)

# a) What percentage of these men are taller than six foot (182.9 cm)?

# compute directly, P(X > 182.9) = 1 - P(X < 182.9)
1-pnorm(182.9,mean=174,sd=6.4)
# [1] 0.08216959

# alternatively, after standardization, P(X > 182.9) = P(Z > z), where  
z <- (182.9 - 174)/6.4
1-pnorm(z)
# [1] 0.08216959

# b) What’s the chance that a randomly selected young man is exactly 170 cm tall?

# P(X = 170) = 0, because X is the continuous random variable  


# c) What’s the chance that a randomly selected young man is 170-something cm tall?

# compute P(170 < X < 180) = P(X < 180) - P(X < 170)
pnorm(180,mean=174,sd=6.4) - pnorm(170,mean=174,sd=6.4)
# [1] 0.5597638

# alternatively, P(170 < X < 180) = P(z1 < Z < z2), where
z1 <- (170 - 174)/6.4
z2 <- (180 - 174)/6.4
pnorm(z2) - pnorm(z1)
# [1] 0.5597638

# d) Find a range of heights that contains 95% of young men. 

# P(X < x) = 0.95 
# find x such that the 95% of young men have heights less than x.
# x is quantile of level 0.95 for N(174, 6.4^2) distribution  

x <- qnorm(0.95,174,6.4)  
x
# [1] 184.5271

# alternatively, 0.95 = P(X < x) = P(Z < (x-174)/6.4) = P(Z < z), where 
# z = (x-174)/6.4  is 0.95-th quantile of the standard normal N(0,1) distribution  
z <- qnorm(0.95)
x <- 174 + z * 6.4
x
# [1] 184.5271

# answer: range of heights less than 184.53 cm contains 
# 95% of the young men 

##------------------------------------------------------------------------------##
# Example. Let X1,X2,...,X25 be independent N(1,4) variables. 
# Let X-bar and S^2 be the sample mean and variance.

# Note, we can use the following results:
# sum_{i=1}^25 Xi ~ N(25, 4*25) = N(25, 100)
# X-bar ~ N(1,4/25)
# (X-bar - 1) / sqrt(4/25) ~ N(0,1)
# (X-bar - 1)/ (S/sqrt(25)) ~ t_24 # t_24 denotes t-distribution with 24 degrees of freedom 

# a) Find P(sum_{i=1}^25 Xi > 30) 
1 - pnorm(30, mean = 25, sd = 10)
# [1] 0.3085375
round(0.3085375,4)
# [1] 0.3085

# or, using standardization, we get 
z <- (30 - 25)/10
1-pnorm(z)

# b) Find P(X-bar < 0)
pnorm(0, mean = 1, sd = sqrt(4/25))
# [1] 0.006209665

pnorm((0-1)/0.4)
#[1] 0.006209665

# c) Find k1 such that P(X-bar < k1) = 0.3
k1 <- qnorm(0.3, mean = 1, sd = 2/5)
k1
# [1] 0.7902398

# or
k1 <- 1 + qnorm(0.3) * 2/5
k1
# [1] 0.7902398

# d) Find k2 such that P((X-bar - 1)/S < k2) = 0.99

# note, 0.99 = P((X-bar - 1)/S < k2) = P(5*(X-bar - 1)/S < 5*k2) = P((X-bar - 1)/(S/5) < 5*k2) 
# let a = 5*k2, P((X-bar - 1)/(S/5) < a) = 0.99, so a is the 0.99 quantile of t_24 distribution  
a <- qt(0.99, df = 24)
k2 <- a/5
k2
# [1] 0.4984319

##------------------------------------------------------------------------------##

speed <- c(57,65,65,55,58,57,71,60,57,65,62,60,54,70,57,52,49,65,68,60,65,68,44)

# compute sample mean (xbar = 60.1739)
mean(speed) 

# give answer correct to 4 decimal places (if needed)
round(mean(speed),4)  

# compute sample standard deviation (s = 6.8068)
sd(speed)

# compute sample variance (s^2 = 46.3320)
var(speed)

# sample size (n=23)
length(speed) 

# compute 90% confidence interval for the true mean µ (mu)
alpha <- 0.10
t <- qt(1-alpha/2, df=length(speed)-1)
t
# note, 1-alpha/2 = 0.95, df = n-1 = 22, t = 1.717144

me <- t*sd(speed)/sqrt(length(speed)) #margin if error 

mean(speed) - me #lower bound
mean(speed) + me #upper bound

# 90% confidence interval for µ (mu) is (57.73676, 62.61107)

##------------------------------------------------------------------------------##

# Calculate 95% confidence interval for the true standard deviation σ (sigma)

## you can clear console (CTRL+L) 
## remove variables from workspace ( rm(alpha) or rm(list=ls()) )

speed <- c(57,65,65,55,58,57,71,60,57,65,62,60,54,70,57,52,49,65,68,60,65,68,44)

alpha <- 0.05
1-alpha/2 #0.975
alpha/2  #0.025

# quantiles of chi-squared distribution with df = n-1 = 22
qchisq(0.975, df=22) # 36.78071
qchisq(0.025, df=22) # 10.98232


sqrt(length(speed)-1)*sd(speed)/sqrt(qchisq(0.975, df=22)) #lower bound
sqrt(length(speed)-1)*sd(speed)/sqrt(qchisq(0.025, df=22)) #upper bound

# 95% confidence interval for σ (sigma) is (5.264315, 9.633961)

##------------------------------------------------------------------------------##

# Conduct hypothesis test H0 : µ = 60 against H1 : µ > 60, use alpha = 0.01 level of significance 

# This is one-sample t-test. 

# observed value of test statistic 
(mean(speed)-60) / (sd(speed)/sqrt(length(speed))) #t_obs = 0.1225337

# H1 : µ > 60, so reject H0 if t_obs > t(n-1,1-alpha)

# quantile t(n-1,1-alpha) equals 
alpha <- 0.01
1-alpha
qt(1-alpha, df=length(speed)-1) # 2.508325
qt(1-alpha, df=length(speed)-1)
# conclusion: there is no evidence to reject the null hypothesis 
# at 1% level of significance  

##------------------------------------------------------------------------------##

# Carry out hypothesis test H0 : µ = 60 against H1 : µ != 60, at alpha = 0.01 level

# observe value of test statistic is the same, t_obs = 0.1225337 

# H1 : µ != 60, so reject H0 if |t_obs| > t(n-1,1-alpha/2)

# quantile t(n-1,1-alpha/2) equals 
alpha <- 0.01
1-alpha/2
qt(1-alpha/2, df=length(speed)-1) # 2.818756

# conclusion: there is no evidence to reject the null hypothesis 
# at 1% level of significance 


##------------------------------------------------------------------------------##

t.test(speed, mu = 60)

#One Sample t-test

#data:  speed
#t = 0.12253, df = 22, p-value = 0.9036
#alternative hypothesis: true mean is not equal to 60
#95 percent confidence interval:
#  57.23045 63.11738
#sample estimates:
#  mean of x 
#60.17391 

##------------------------------------------------------------------------------##

t.test(speed, mu = 60, conf.level = 0.90)

#One Sample t-test

#data:  speed
#t = 0.12253, df = 22, p-value = 0.9036
#alternative hypothesis: true mean is not equal to 60
#90 percent confidence interval:
#  57.73676 62.61107
#sample estimates:
#  mean of x 
#60.17391 

##------------------------------------------------------------------------------##

t.test(speed, mu = 60, alternative = "greater", conf.level = 0.99)

#One Sample t-test

#data:  speed
#t = 0.12253, df = 22, p-value = 0.4518
#alternative hypothesis: true mean is greater than 60
#99 percent confidence interval:
#  56.61383      Inf
#sample estimates:
#  mean of x 

##------------------------------------------------------------------------------##









# set your working directory (your csv file is in working directory)
setwd("...") 

#read data from csv file and store as a data frame (df) named speed_df
speed_df <- read.csv('speed.csv')  
speed_df # print data frame

mean(speed_df$speed) #compute mean of variable speed
sd(speed_df$speed) #compute standard deviation of variable speed

#you can save speed_df$speed as speed and then compute its mean and variance
speed <- speed_df$speed 
mean(speed)
var(speed)

#### alternatively, select your data file and read it in R 
my_data <- read.csv(file.choose())

##------------------------------------------------------------------------------##


