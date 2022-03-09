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
# Problem 1
#####################

# load data
load("/Users/user/Desktop/Semester 2/Applied Stats II/GitHub/StatsII_Spring2022/problemSets/PS2/template/climateSupport.RData")
climateSupport

# Outcome variable = choice
climateSupport$choice
# Explanatory variables = countries, sanctions
climateSupport$sanctions
climateSupport$countries
# PART 1: Remember, we are interested in predicting the likelihood of an individual supporting a policy based 
# on the number of countries participating and the possible sanctions for non-compliance. 

# Fit an additive model. Provide the summary output, the global null hypothesis, and p-value. Please describe the results and provide 
# a conclusion.

# Alt method: Load package 'mgcv' and use gam function to run additive model
library(mgcv)
alt_additive_model <- gam(choice ~ ., family = 'binomial', data = climateSupport)
summary(alt_additive_model)
?gam

# Conventional method: Use glm() function
additive_model <- glm(choice ~ ., family = 'binomial', data = climateSupport)
summary(additive_model)

# PART 2: If any of the explanatory variables are significant in this model, then:

# (a) For the policy in which nearly all countries participate [160 of 192], how does increasing sanctions from 
# 5% to 15% change the odds that an individual will support the policy? (Interpretation of a coefficient)

# R METHOD

# Try for 5% sanctions
high_participation <-  climateSupport[climateSupport$countries == '160 of 192',]
fivepercent_data <- high_participation[high_participation$sanctions == '5%',]
fivepercent_data
# Use predict function with 'response' type to estimate probabilities
fivepercent_probabilities <- predict(additive_model, newdata = fivepercent_data, type = "response")
summary(fivepercent_probabilities) # mean = 0.6382 
0.6382*100 # Probability = [1] 63.82%
100-63.82 # [1] 36.18%
# The odds ratio is...
63.82/36.18 # [1] Odds = 1.763958/1 
# For the 160/192 policy, when 5% sanctions are applied, the odds of support is 1.763958/1

# Now try for 15% sanctions...
fifteenpercent_data <- high_participation[high_participation$sanctions == '15%',]
fifteenpercent_data
# Use predict function with 'response' type to estimate probabilities
fifteenpercent_probabilities <- predict(additive_model, newdata = fifteenpercent_data, type = "response")
summary(fifteenpercent_probabilities) # mean = 0.5603
0.5603*100 # Probability = [1] 56.03%
100-56.03 # [1] 43.97
# The odds ratio is...
56.03/43.97 # [1] Odds = 1.274278/1 
# For the 160/192 policy, when 15% sanctions are applied, the odds of support is 1.274278/1

# Overall, the majority support of countries increases the odds of support for agreements. Sanctions also have an effect
# In answer to the question posed, when 5% sanctions rise to 15% for the 160/192 policy, the odds of support declines from 1.76/1 to 1.27/1
# There is a decrease in odds of support by 0.48968 when this is so...
1.763958-1.274278 # = 0.48968


# (b) For the policy in which very few countries participate [20 of 192], how does in-creasing sanctions from 5% 
# to 15% change the odds that an individual will support the policy? (Interpretation of a coefficient)

# R METHOD

# Try for 5% sanctions
low_participation <-  climateSupport[climateSupport$countries == '20 of 192',]
fivepercent_data2 <- low_participation[low_participation$sanctions == '5%',]
fivepercent_data2
# Use predict function with 'response' type to estimate probabilities
fivepercent_probabilities2 <- predict(additive_model, newdata = fivepercent_data2, type = "response")
summary(fivepercent_probabilities2) # mean = 0.4798
0.4798*100 # Probability = [1] 47.98%
100-47.98 # [1] 52.02
# The odds ratio is...
47.98/52.02 # [1] Odds = 1/0.9223376
# For the 160/192 policy, when 5% sanctions are applied, the odds of support is 1/0.9223376

# Now try for 15% sanctions...
fifteenpercent_data2 <- low_participation[low_participation$sanctions == '15%',]
fifteenpercent_data2
# Use predict function with 'response' type to estimate probabilities
fifteenpercent_probabilities2 <- predict(additive_model, newdata = fifteenpercent_data2, type = "response")
summary(fifteenpercent_probabilities2) # mean = 0.3999
0.3999*100 # Probability = [1] 39.99%
100-39.99 # [1] 60.1%
# The odds ratio is...
39.99/60.1 # [1] Odds = 0.665391/1 
# For the 160/192 policy, when 15% sanctions are applied, the odds of support is 0.665391/1 

# Therefore, when 5% sanctions rise to 15% for the 20/192 policy, the odds of support declines from 0.92/1 to 0.67/1
# Decrease in odds of support by 0.25
0.92-0.67 # = 0.48968

# Overall, the odds of support for an agreement with only 20 particpating countries is always less likely than 
# the odds that it recieves no support, regardless of sanction percentages. 
# Therefore, in conjunction with results from question (a), country participation is likley an explanatory variable
# of support. However, the effect of sanctions (from 5-15%) does decrease support further, much-like in question (a)
# Sanctions therefore might have an effect also. 

# (c) What is the estimated probability that an individual will support a policy if there are 80 of 192 countries 
# participating with no sanctions?
View(climateSupport)

# Create est_data dataframe to make predictions
no_sanctions <-  climateSupport[climateSupport$sanctions == 'None',]
est_data <- no_sanctions[no_sanctions$countries == '80 of 192',]
est_data

# Use predict function with 'response' type to estimate probabilities
est_probabilities <- predict(additive_model, newdata = est_data, type = "response")
est_probabilities
summary(est_probabilities) # mean = 0.5159
# Estimated probability = 0.5159 or 51.59/100%
# Approx. a 52% chance that an individual will support a policy if there are 80 of 192 countries 
# participating with no sanctions


# (d) Would the answers to 2a and 2b potentially change if we included the interaction term in this model? Why?
  # • Perform a test to see if including an interaction is appropriate.

# Create interaction model by multiplying x1 (countries) by x2 (sanctions)
interaction_model <- glm(choice ~ countries*sanctions, family = 'binomial', data = climateSupport)
interaction_model$coefficients

# R METHOD

# FOR PART (a)  USING INTERACTION MODEL
# Use high_particpation, fivepercent_data and fifteen_percent_data objects from question (a)
# Use predict function with 'response' type to estimate probabilities
qa_interact_probabilities <- predict(interaction_model, newdata = fivepercent_data, type = "response")
summary(qa_interact_probabilities) # mean = 0.6433
0.6433*100 # Probability = [1] 64.33%
100-64.33 # [1] 35.67%
# The odds ratio is...
64.33/35.67 # [1] Odds = 1.803476/1 using interaction model
# This is marginally greater than 1.763958/1 odds observed in part (a)

# Now try for 15% sanctions on question (a)...
# Use predict function with 'response' type to estimate probabilities
qa_interact_probabilities2 <- predict(interaction_model, newdata = fifteenpercent_data, type = "response")
summary(qa_interact_probabilities2) # mean = 0.5472
0.5472*100 # Probability = [1] 54.72%
100-54.72 # [1] 45.28
# The odds ratio is...
54.72/45.28 # [1] Odds = 1.208481/1 using interaction model
# This is slightly lower than 1.274278/1 odds observed in part (a)

# Therefore, when 5% sanctions rise to 15% for the 160/192 policy using an interactive model, the odds of support still declines 
# from 1.803476/1 to 1.208481/1 
1.803476-1.208481
# Decrease in odds of support by 0.594995. This is in comparison with a lesser effect seen in part a at 0.48968

# Overall, results mixed as intercation increases support marginally with low sanctions but lessens support slightly
# for high sanctions. Results also quite close to original model without intercation. Regardless, there is an overall
# effect on the odds when including the interaction as support was decreased by a greater margin than original model
# wgen sanctions applied. 

# FOR PART (B) USING INTERACTIVE MODEL
# Use low_particpation, fivepercent_data and fifteen_percent_data objects from question (b)
# Use predict function with 'response' type to estimate probabilities
qb_interact_probabilities <- predict(interaction_model, newdata = fivepercent_data2, type = "response")
summary(qb_interact_probabilities) # mean = 0.4618
0.4798*100 # Probability = [1] 46.18%
100-46.18 # [1] 53.82
# The odds ratio is...
46.18/53.82 # [1] Odds = 1/0.8580453
# For the 160/192 policy, when 5% sanctions are applied, the odds of support is 1/0.8580453

# This is a decrease in odds from the results of question (b) 1/0.92


# Now try for 15% sanctions...
# Use predict function with 'response' type to estimate probabilities
qb_interact_probabilities2 <- predict(interaction_model, newdata = fifteenpercent_data2, type = "response")
summary(qb_interact_probabilities2) # mean = 0.4082
0.4082*100 # Probability = [1] 40.82%
100-40.82 # [1] 59.18%
# The odds ratio is...
40.82/59.18 # [1] Odds = 0.6897601/1 
# For the 160/192 policy, when 15% sanctions are applied, the odds of support is 0.6897601/1 
# This is an increase in support from the odds of question (b) 1/0.67

# Therefore, when 5% sanctions rise to 15% for the 20/192 policy using an interactive model, the odds of support still declines 
# from 1/0.8580453 to 0.6897601/1 
0.8580453-0.6897601
# Decrease in odds of support by 0.1682852

# These results using an interactive model differ from odds in part (b) of 1/0.92 and 1/0.67 
# However, the results here are still very similar. Main difference is that effect of sanction rise on support 
# is lower at 0.1682852

# Overall, the effect of including an intercation term in the model effects the answers to part a and b, but also slightly.
# This indicates that an intercation term may not be neccesary as its effect was not so large. 











