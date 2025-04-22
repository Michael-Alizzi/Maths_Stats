
t_crit_funct <- function(alpha, df){
  
  critical_value_left <- qt(alpha, df)
  
  critical_value_right <- qt(1 - alpha, df)
  
  critical_value_two_sided <- qt(1 - alpha/2, df)
  
  return(list(right_tail_critical = critical_value_right, 
              left_tail_critical = critical_value_left, 
              two_tail_critical = critical_value_two_sided))
  
}

attr(t_crit_funct, "Critical Values for t-test") <- 
"Left, right and two tailed critical values for a t-test"

t_test_funct <- function(data, mean, alpha, tail = 0){
  
  if(tail == 0){ 
    
    t_stat <- t.test(x = data, mu = mean, conf.level = 1-alpha, alternative = "less")
    
  } else if(tail == 1) {
    
    t_stat <- t.test(x = data, mu = mean, conf.level = 1-alpha, alternative = "greater")
    
  } else {
    
    t_stat <- t.test(x = data, mu = mean, conf.level = 1-alpha, alternative = "two.sided")
    
  }
  
  return(list(
    t_stat = t_stat$statistic,
    p_value = t_stat$p.value,
    df = t_stat$parameter,
    conf_int = t_stat$conf.int
  ))
  
}

attr(t_test_funct, "Full t-test") <- 
  "Use if you have sample data.
Supports left-tailed, right-tailed, and two-tailed t-tests.
- When tail = 0 (default), it performs a left-tailed test (alternative = 'less').
- When tail = 1, it performs a right-tailed test (alternative = 'greater').
- When tail = 2, it performs a two-tailed test (alternative = 'two.sided')."

stat_manual <- function(X_bar, mean, std, n){
  
  stat <- (X_bar-mean)/(std/sqrt(n))
  
  return(stat)
  
}

attr(stat_manual, "T or Z statistic") <- 
  "
  Calculates a T or Z statistic based on inputs. 
- X_bar: sample mean
- mean: hypothesised population mean (from H0)
- std: standard deviation (use population std dev for Z, sample std dev for T)
- n: sample size

Returns the appropriate test statistic depending on context."



t_stat_comm_var_funct <- function(X_bar, Y_bar, s_p, nY, nX){
  
  t_stat <- (X_bar-Y_bar)/(s_p*((1/nY)+(1/nX))^0.5)
  
  return(t_stat)
  
}

attr(t_stat_comm_var_funct, "Common Variance t-statistic") <- 
"Computes the t-statistic for a two-sample independent t-test assuming equal (pooled) variance. 
X_bar and Y_bar are the sample means, s_p is the pooled standard deviation, 
and nY and nX are the sample sizes for groups Y and X respectively. 
To align the direction of the t-statistic with your hypothesis, place the group you are testing as being greater as the 'X' sample."

t_test_conf_int_funct <- function(t_stat, std, n, mean){
  
  me <- t_stat*std/sqrt(n)
  
  L_bound <- mean - me 
  U_bound <- mean + me 
  
  
  return(list(lower_bound = L_bound,
              upper_bound = U_bound))
  
}

attr(t_test_conf_int_funct, "Confidence Interval for t-test") <- 
"Calculates upper and lower bounds using a t-test, given the t-statistic (t-stat), standard deviation (std), sample size (n) and the average of the sample 
(mean)."

t_test_p_stat_funct <- function(t_stat, n1, n2 = NA, tail = 0){
  
  if(is.na(n2)){
    
    df <- n1-1
  } else {
    
    df <- n1+n2-2
    
  }
  
  if(tail == 0){ 
    
    p_value <- pt(t_stat, df, lower.tail = TRUE)
    
  } else if(tail == 1) {
    
    p_value <- pt(t_stat, df, lower.tail = FALSE)
    
  } else {
    
    p_value <- 2 * min(
      pt(t_stat, df, lower.tail = TRUE),
      pt(t_stat, df, lower.tail = FALSE)
    )
    
  }
  
  return(p_value)
  
}

attr(t_test_p_stat_funct, "p-value for t-test") <- 
"Left, right and two tailed p-value using a t-test. When the tail parameter = 0 (default) , it uses the left tailed t-test (less than). When the tail 
parameter = 1, it uses the right tailed t-test (greater than). When the tail parameter = 2, it uses the two tailed t-test (equal to). The n1 parameter is 
the sample size and if comparing two samples, set the n2 paramater to the sample size of the second sample."

f_crit_funct <- function(alpha, df1, df2){
  
  critical_value_left <- qf(alpha, df1, df2)
  
  critical_value_right <- qf(1 - alpha, df1, df2)
  
  critical_value_two_sided <- c(
    lower = qf(alpha / 2, df1, df2),
    upper = qf(1 - alpha / 2, df1, df2)
  )
  
  return(list(right_tail_critical = critical_value_right, 
              left_tail_critical = critical_value_left, 
              two_tail_critical = critical_value_two_sided))
  
}

attr(f_crit_funct, "Critical Values for f-test") <- 
"Left, right, and two-tailed critical values for an f-test, given numerator (df1) and denominator (df2) degrees of freedom. 
Set the group with the larger sample variance as the numerator (df1)."

f_test_funct <- function(x, y, alpha){
  
  test <- var.test(x, y, conf.level = 1 - alpha)
  
  f_stat <- test$statistic
  p_value <- test$p.value
  df <- test$parameter
  conf_int <- test$conf.int
  
  return(list(
    f_stat = f_stat,
    p_value = p_value,
    df = df,
    conf_int = conf_int
  ))
}

attr(f_test_funct, "Full F-test") <- 
  "Performs an F-test to compare the variances of two independent samples.
- x: numeric vector of sample 1
- y: numeric vector of sample 2
- alpha: significance level (e.g. 0.05 for 95% confidence)
Returns a list containing:
- f_stat: observed F-statistic
- p_value: p-value of the test
- df: degrees of freedom for both groups
- conf_int: confidence interval for the ratio of variances"

f_test_conf_int_funct <- function(alpha, nX, nY, f_ratio){
  
  f_test_lower <-(1/qf(1-alpha/2, nX-1, nY-1)) # Lower critical value
  
  f_test_upper <- qf(1-(alpha/2), nX-1, nY-1) # upper critical value 
  
  lower_b <- f_ratio*f_test_lower
  
  upper_b <- f_ratio*f_test_upper
  
  return(list(
    f_lower_critical_value = f_test_lower,
    f_upper_critical_value = f_test_upper,
    f_lower_confidence_interval = lower_b,
    f_upper_confidence_interval = upper_b
  ))
}

attr(f_test_conf_int_funct, "Confidence Interval for f-test") <- 
"Use if you have sample data. Calculates upper and lower bounds using a t-test, given the alpha, sample size of x (nX), the sample size of y (nY) and the f-ratio (the larger sample 
variance, S2x divided by the smaller sample variance S2y)."

f_test_p_stat_funct <- function(f_ratio, n1, n2, tail = 0){
  
  if(tail == 0){ 
    
    p_value <- pf(f_ratio, df1 = n1 - 1, df2 = n2 - 1)  
    
  } else if(tail == 1) {
    
    p_value <- 1-pf(f_ratio, n1-1, n2-1) 
    
  } else {
    
    p_value <- 2 * min(pf(f_ratio, n1-1, n2-1), 1 - pf(f_ratio, n1-1, n2-1))
    
  }
  
  return(p_value)
  
}

attr(f_test_p_stat_funct, "p-value for f-test") <- 
"Left, right and two tailed p-value using an f-test. When the tail parameter = 0 (default) , it uses the left tailed f-test (less than). When the tail 
parameter = 1, it uses the right tailed f-test (greater than). When the tail parameter = 2, it uses the two tailed f-test (equal to). The f-ratio parameter 
being the larger sample variance (S2x) divided by the smaller sample variance (S2y)."