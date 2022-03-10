#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
##### Problem 1 #####
#####################

# Write an R function that implements this test where the reference distribution is normal.
# Set the seed
set.seed(123)

# 1) Data generation
# Create 1000 Cauchy random variables and 1000 normal variables using rcauchy and rnorm functions
x <- (rcauchy(1000, location = 0, scale = 1))
x
y <- rnorm(1000)
y
# Make dataframe from x and y
data <- data.frame(x, y)

# 2) Find empirical and theoretical CDFs
# Create empirical distribution of observed data using ecdf() function
ECDF <- ecdf(data$x) 
ECDF
empiricalCDF <- ECDF(data$x)
empiricalCDF
# Create theoretical data with normal distribution using ecdf() function 
TCDF <- ecdf(data$y)
TCDF
theoreticalCDF <- TCDF(data$y)
theoreticalCDF
# ALT method: Can also use pnorm(data, mean, sd) instead of ecdf for reference distribution
mean(data$y) # [1] 0.03609765
sd(data$y) # [1] 1.003827
TCDF2 <- pnorm(data$y, 0.03609765, 1.003827)
TCDF2
theoreticalCDF2 <- TCDF(data$y)
theoreticalCDF2 # Same results as using ecdf() function 

# 3) Calculate T-sat named D
D <- max(abs(empiricalCDF - pnorm(theoreticalCDF)))
D # [1] 0.8344237

# 4) Calculate p-value + Draw Conclusions
# Install package 'dgof' to use ks.test function
install.packages('dgof')
library('dgof')
ks.test(empiricalCDF, theoreticalCDF) # p-value = 1
ks.test(empiricalCDF, 'pnorm') # D = 0.5004, p-value < 2.2e-16
# P value far greater than 0.5, therefore indicating that ecdf is far from normal. 


#####################
#### Problem 2 ######
#####################

# Estimate an OLS regression in R that uses the Newton-Raphson algorithm 
# (specifically BFGS, which is a quasi-Newton method), and show that you get the equivalent 
# results to using lm. Use the code below to create your data.

set.seed (123)
# Create dataframe with x and y variables
data <- data.frame(x = runif(200, 1, 10)) 
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Using lm() function/OLS method
lm <- lm(data$y ~ data$x)
lm
# Coefficients:
  # (Intercept)       data$x  
  # -0.5055       2.8166  

# Try alternate 'BFGS' method
min_rss <- function(data, par) { # Function to minimise the residual sum of squares
  with(data, sum((par[1] + par[2] * x - y)^2))
}
# Using optim() function following quasi-Newton BFGS method 
?optim
BFGS_result <- optim(par = c(0, 1), fn = min_rss, data = data, gr=NULL, method = 'BFGS', lower = -Inf, upper = Inf, control = list(), hessian = FALSE)
BFGS_result
# [1] -0.5055275  2.8165749
# Coefficients using BFGS method equivalent to lm() function output. 





