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
library(stargazer)
# Load dataset 
data <- eha::infants
stargazer(data[1:4,], summary=FALSE, rownames=FALSE)
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

## Plot the fitted survival model
ggsurvplot(survfit(cox_mod), data = data, color = "#2E9FDF",
           ggtheme = theme_minimal())


stargazer::ggsurvplot(survfit(cox_mod), data = data, color = "#2E9FDF",
                      ggtheme = theme_minimal())

