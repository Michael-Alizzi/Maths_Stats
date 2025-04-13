# Pull all functions
source('~/Maths_Stats/Packages.R')
source('~/Maths_Stats/Probability Functions.R')
source('~/Maths_Stats/Random Variable Functions.R')
source('~/Maths_Stats/Distribution Functions.R')
source('~/Maths_Stats/Transformations.R')
source('~/Maths_Stats/Random Samples.R')
source('~/Maths_Stats/Confidence Intervals and Hypothesis Tests.R')

options(scipen = 999)

# Function metadata
attributes(f_test_p_stat_funct)

library('dplyr')
library('openxlsx')
library('plotly')
library('webshot')
library('htmlwidgets')

h_data <- read.xlsx('Housing prices.xlsx')

colnames(h_data) <- c("House", "SellingPrice", "Location", "NumberofRooms")

plot(multi_linear$fitted.values, multi_linear$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "blue")  # smaller blue dots
abline(h = 0, col = "red", lwd = 2)


plot(log_linear$fitted.values, log_linear$residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "blue",
     ylim = range(c(log_linear$residuals, 0)))  # ensures 0 is included
abline(h = 0, col = "red", lwd = 2)

boxplot(h_data$SellingPrice, main = "Boxplot with Outliers")

boxplot(log(h_data$SellingPrice), main = "Boxplot with Outliers")

boxplot.stats(h_data$NumberofRooms)$out
