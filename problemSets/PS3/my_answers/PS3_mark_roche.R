########### PROBLEM SET 3 #########
### Mark Roche ####
setwd(getwd())

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



###    1. Construct and interpret an unordered multinomial logit with GDPWdiff as the output and ”no change” 
# as the reference category, including the estimated cutoff points and coefficients.


###### PROBLEM 1 #############

# Libraries
library(MASS)
library(nnet)
library(ggplot2)

data <- read.csv('https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2022/main/datasets/gdpChange.csv')
data

# Run descriptive stats on dataset
summary(data)
ftable(xtabs(~ OIL + REG, data = data))
# Vast majority of fuel exports do not exceed 50%; by comparsion there is a similar sample of 
# democratic and non-democratic countries. 

# Manipulate GDPWdiff variable to make categories
# Assign all negative values to 'negative' category, all positives to 'positive', and all 
# zeroes to 'no change' category
data$GDPWdiff[data$GDPWdiff > 0] <- 'positive'
data$GDPWdiff[data$GDPWdiff < 0] <- 'negative'
data$GDPWdiff[data$GDPWdiff == 0] <- 'no change'
data
# Set reference category to no change
data$GDPWdiff <- as.factor(data$GDPWdiff)
data$GDPWdiff <- relevel(data$GDPWdiff, ref = 'no change')

# Create two dummy variables out of categories for REG
# Name them Democracy and Non-Democracy
democracy <- ifelse(data$REG == 1, 1, 0)
non_democracy <- ifelse(data$REG == 0, 1, 0)

# Create two dummy variables out of categories for OIL exports
# Name them large_oilex and small_oilex
large_oilex <- ifelse(data$OIL == 1, 1, 0)
small_oilex <- ifelse(data$OIL == 0, 1, 0)

final_data <- data.frame(GDPWdiff = data$GDPWdiff,
                         large_oilex = large_oilex,
                         small_oilex = small_oilex,
                         democracy = democracy,
                         non_democracy = non_democracy)
final_data
# Run multinomial unorderd logit
unorder_reg <- multinom(GDPWdiff ~ large_oilex + small_oilex + democracy + non_democracy, data = final_data)
summary(unorder_reg)

# Coefficients:
  #           (Intercept) large_oilex small_oilex
# negative    5.145122    8.385181   -3.240059
# positive    5.554894    8.486243   -2.931349
#           democracy non_democracy
# negative  3.248796      1.896326
# positive  3.648636      1.906258        

##### Interpretating  coefficients and cuttoff points in the model

## 1) The Coefficients
#  A one-unit increase in the variable large_oilex is associated with a large increase in  
# log odds of being negative difference between GDP rather than no change GDP difference 
# in the amount of 8.385.
# A one-unit increase in the variable large_oilex is associated with a large increase in  
# log odds of being positive difference between GDP rather than no change GDP difference 
# in the amount of 8.486.

#  A one-unit increase in the variable small_oilex is associated with a decrease in  
# log odds of being negative difference between GDP rather than no change GDP difference 
# in the amount of -3.240
# A one-unit increase in the variable small_oilex is associated with a decrease in  
# log odds of being positive difference between GDP rather than no change GDP difference 
# in the amount of -2.931

# A one-unit increase in the variable democracy is associated with an increase in  
# log odds of being negative difference between GDP rather than no change GDP difference 
# in the amount of 3.249
# A one-unit increase in the variable democracy is associated with an increase in  
# log odds of being positive difference between GDP rather than no change GDP difference 
# in the amount of 3.649

# A one-unit increase in the variable non_democracy is associated with a slight increase in  
# log odds of being negative difference between GDP rather than no change GDP difference 
# in the amount of 0.327
# A one-unit increase in the variable non_democracy is associated with a slight increase in  
# log odds of being positive difference between GDP rather than no change GDP difference 
# in the amount of 0.326

## 2) The Estimated Cutoff Points:
# The intercept for negative GDP diff: when all predictors are at 0, the log odds of there being negative 
# difference between GDP rather than no change GDP difference is associated with an increase in the amount 
# of  5.554894
# The intercept for positive GDP diff: when all predictors are at 0, the log odds of there being positive 
# difference between GDP rather than no change GDP difference is associated with an increase in the amount 
# of 5.145122


# Run Z-test to get p-value. 
# This is needed to see whether these coefficents are statistically significant within
# the model.
z <- summary(unorder_reg)$coefficients/summary(unorder_reg)$standard.errors
z
# Pvalue
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#           (Intercept) large_oilex small_oilex    democracy non_democracy
# negative      0           0           0         1.347811e-13  6.367129e-09
# positive     0           0           0         0.000000e+00  4.865686e-09

# From this table, we can see that the coefficients for oil exports (large and small) and one coefficient for democracy are 
# statistically significant
# We can also infer that the coefficients for non_democracy appear to not be. 
# The intercept is also below 0.05 at 0. It is therefore statistically significant
# Therefore, under this model, only the variables democracy, large_oilex and small_oilex should be kept for further analyses

# We can therefore run a final more refined model
final_model <- multinom(GDPWdiff ~ large_oilex + small_oilex + democracy - non_democracy, data = final_data) # subtract non_democracy from model
summary(final_model)
# Covert coefficients to odds through exponation to make further interpretation on non_democracy 
# and oil predictors
exp(coef(final_model))

## Interpret Coefficients

# For Democracy Variable:
# There is an increase in the reference category odds that there will be a negative GDP difference year-on-year by 3.867 times when a country is a democracy 
# There is an increase in the reference category odds that there will be a positive GDP difference year-on-year by 5.710 times when a country is a democracy 

# For large_oilex variable:
# There is an increase in the reference category odds that there will be a negative GDP difference year-on-year by 3518.108 times when fuel exports
# exceed more than 50% of total exports
# There is an increase in the reference category odds that there will be a positive GDP difference year-on-year by 3905.215 times when fuel exports
# exceed more than 50% of total exports

# For large_oilex variable:
# There is an increase in the reference category odds that there will be a negative GDP difference year-on-year by 0.113 times when fuel exports
# are less than 50% of total exports
# There is an increase in the reference category odds that there will be a positive GDP difference year-on-year by 0.154 times when fuel exports
# are less than 50% of total exports



###  2. Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, 
  # including the estimated cutoff points and coefficients.

# Order GDPWdiff by levels positive, then no change and finally negative 
data$GDPWdiff <- factor(data$GDPWdiff, levels = c('positive', 'no change', 'negative'),
                               labels = c('postive', 'no change', 'negative'))
data$GDPWdiff

## Run ordered logit regression using polr() function from MAAS package
order_reg <- polr(GDPWdiff ~ ., data = final_data, Hess = TRUE)
summary(order_reg)

              Ordinal Regression Output 
Call:
  polr(formula = GDPWdiff ~ ., data = final_data, Hess = TRUE)

Coefficients:
              Value   Std. Error t value
OIL           -0.1788    0.11546  -1.549
REG           0.4102    0.07518   5.456

Intercepts:
                    Value    Std. Error t value 
no change|negative  -5.3199   0.2523   -21.0865
negative|positive   -0.7036   0.0476   -14.7932

Residual Deviance: 4686.606 
AIC: 4694.606 

# Significance Test: Calculate a p value
coef_table <- coef(summary(order_reg))
pval <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

(coef_table <- cbind(coef_table, "p value" = pval))
### P values for both coefficients are significant at 1.214075e-01 and 
# 4.875321e-08

### CONVERT TO ODDS RATIO TO INTERPRET COEFFICIENTS
# exponate coefficients
exp((order_reg))
large_oilex   democracy 
0.8362455   1.5070726 

# For countries that export oil exceeding 50% of GDP, the odds of there being a positive GDP-differenceversus negative
# year-on-year is 0.84 times that of those whose oil exports do not exceed 50%, holding other variables constant.

# For countries whose regime (REG) is democratic, the odds of there being a positive GDP-difference versus negative
# year-on-year is 1.5 times that of undemocratic regimes, holding other variables constant.



######### PROBLEM 2 #########

# (a) Run a Poisson regression because the outcome is a count variable. Is there evidence that PAN 
  # presidential candidates visit swing districts more? Provide a test statistic and p-value.
dataset <- read.csv('https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2022/main/datasets/MexicoMuniData.csv')
dataset
# Outcome variable:
# Variable PAN.visits.06 is a count variable that requires poission regression. 
dataset$PAN.visits.06
class(dataset$PAN.visits.06)
# Predictors:
# Main predictor of interest is whether the district was highly contested, or whether it was not 
# (the PAN or their opponents have electoral security) in the previ-ous federal elections during 2000 
# (competitive.district), which is binary (1=close/swing district, 0=”safe seat”). 
dataset$competitive.district
# We also include marginality.06 (a measure of poverty) and PAN.governor.06 (a dummy for whether 
# the state has a PAN-affiliated governor) as ad-ditional control variables.
dataset$marginality.06
dataset$PAN.governor.06
dataset$swing <- ifelse(dataset$competitive.district == 1, 1, 0)
dataset$safe <- ifelse(dataset$competitive.district == 0, 1, 0)

dataset <- data.frame(PAN.visits.06 = dataset$PAN.visits.06,
                         competitive = dataset$competitive.district,
                         marginality = dataset$marginality.06,
                      governor = dataset$PAN.governor.06)

##### RUN POISSION REGRESSION
poisson_reg <-glm(formula = PAN.visits.06 ~ ., data = dataset,
             family = poisson)
summary(poisson_reg)

# Intercept and coefficient for marginality appear highly significant. 
# Coefficient for competitive district variable is -0.08135. 

# Exponate the coefficients to make interpretation
exp(coef(poisson_reg))
# Exponated coefficient for districts (swing or safe) is 0.922
# Therefore the expected difference in log count between swing districts compared to safe districts is 0.922

# Therefore, it does appear that PAN candidates visit swing districts more often than safe districts. 

# (b) Interpret the marginality.06 and PAN.governor.06 coefficients.
summary(poisson_reg) 
# For a one  unit increase in marginality, the difference in the logs of expected counts is expected to 
# change by -2.08014, given the other predictors are held constant.
# PAN.governor.06 coefficient only slightly negatively correlated with outcome variable (-0.31) 
# Overall, both coefficients are negative indicting a negative relation. 
# However, unlike PAN.governor.06, the coefficient for marginality is highly significant with a p-value of
# >0.001

# (c) Provide the estimated mean number of visits from the winning PAN presidential candi-
# date for a hypothetical district that was competitive (competitive.district=1), had
# an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).

competitive <- dataset$competitive
marginality <- dataset$marginality
governor <- dataset$governor
# Create new data
newdata <- data.frame(competitive == 1,
                      governor == 1, 
                      marginality == 0)
newdata
# Make the prediction on newdata using predict() function
pred <- predict(poisson_reg, newdata = newdata, type = "response")
mean(pred) # [1] 0.09181554

# Mean visits from a winning candidate for competitive district with an avg. poverty level 
# and a PAN governor is 0.092. 
  
