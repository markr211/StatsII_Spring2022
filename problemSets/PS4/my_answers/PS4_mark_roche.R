######## PROBLEM SET 4 #######

#### Mark Roche ####

### PROBLEM 1 ####

# We’re interested in modeling the historical causes of infant mortality. 
# We have data from 5641 first-born in seven Swedish parishes 1820-1895. 
# Using the ”infants” dataset in the eha library, fit a Cox Proportional Hazard model 
# using mother’s age and infant’s gender as covariates. Present and interpret the output.

## Install Libraries
library(eha)
library(survival)
library(survminer)

# Load dataset 
data <- eha::infants

# Call help file on dataset and analyse outocme and predictors
?eha::infants
# Predictors: sex is a binary predictor with options boy/girl; Mothers age at infants birth a numerical 
# predictor
## Outcome variable: enter, exit and event; charts whether an infant died (i.e the event) and over what time (enter and exit). 

#### COX PROPORTIONAL MODEL ###
### Fit Cox Proportional Hazard Model  
inf_surv <- with(data, Surv(enter, exit, event))
cox_mod <- coxph(inf_surv ~ age + sex, data = data)
summary(cox_mod)

## ### INTERPRETING THE OUTPUT 

### The sample size of infants is 105. The no. of deaths is 21

## Coefficients:
## Coefficient for sexboy is -0.04. The expected log of the hazard decreases by -0.04 when an infant is a boy
# compared to a girl, holding age constant.
## Coefficient for age is -0.49. The expected log of the hazard decreases by -0.49 if a mother is one year older, 
# holding the sex of the infant constant.

## Exponate the coefficients for hazard ratios. This information is already given in the summary output.
## Both hazard ratios are below 1 (i.e. they are associated with increased survivablity). 
# Hazard ratio for sexboy (i.e. exp(coef)) = 0.62. Therefore, the hazard ratio for boys as opposed to girls is 
# 62%; in other words, out of 100 infants, 62 that die are most likley to be male as to apposed to 58 females. 
## Hazard ratio for age = 0.96. 

## P-values: None of the coefficients are statistically significant

### Run Chisq test
drop1(cox_mod, test = "Chisq")
## Both p-values are < 0.05. There is therefore evidence that we can fail to reject the null hypothesis
## that the hazards are proportional.

cox_fit <- survfit(cox_mod)
autoplot(cox_fit)
cox_fit
## Plot the model
ggsurvplot(survfit(cox_mod), color = "#2E9FDF",
           ggtheme = theme_minimal())


### INRERACTION MODEL ###
### Try an interaction between mother's age and the sex variable
cox_interact <- coxph(Surv(exit, event) ~ age*sex, data = data)
summary(cox_interact)

### Run Chisq test
drop1(cox_interact, test = "Chisq")

### Model is not significant as p-value < 0.05

Call:
  coxph(formula = Surv(exit, event) ~ age * sex, data = data)

n= 105, number of events= 21 

              coef    exp(coef) se(coef)  z      Pr(>|z|)
age         0.03092   1.03141  0.05637  0.549    0.583
sexboy      3.34675  28.41019  2.56191  1.306    0.191
age:sexboy -0.14706   0.86324  0.09723 -1.512    0.130

          exp(coef)       exp(-coef)   lower .95      upper .95
age           1.0314     0.9696    0.9235     1.152
sexboy       28.4102     0.0352    0.1874  4306.996
age:sexboy    0.8632     1.1584    0.7135     1.044

Concordance= 0.621  (se = 0.063 )
Likelihood ratio test= 3.99  on 3 df,   p=0.3
Wald test            = 3.38  on 3 df,   p=0.3
Score (logrank) test = 3.56  on 3 df,   p=0.3



