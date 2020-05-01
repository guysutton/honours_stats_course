###########################################################
###########################################################
# - Introduction to general linear models (GLM)
# - Honours Class Tutorial
# - Rhodes Universtiy
# - Script written: 12/02/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# You have already been introduced to linear models earlier in this course.
# Linear models try to predict some response variable by some explanatory variable. 
# For example, you could try to predict the number of offspring produced by an insect
# at different temperatures and CO2 levels. 
# Linear models allow us to evaluate whether there is a statistical association between
# our response variable (no. of offspring) and the explanatory factors (temp and/or CO2). 
# ANOVA is just a linear model with categorical factors (e.g. CO2). 

# Assumtpions of linear models:
# 1. Relationship between dependent and independent variables is linear. 
# 2. Residuals follow a normal distribution. 
# 3. Homogeneity of variances - residuals are assumed to have constant variance. 

##################################
# - Linear model assumptions  
##################################

# 1. Linearity assumption - we can use the residuals vs fitted plot 
#    - We want to see no pattern (the red line should be straight - horizontal). 
#    - This is indicative of a linear relationship between our explanatory variables
#    - and our response variable. 

# 2. Residuals follow a normal distribution 
#    - We want to see the points fall approximately along the reference 1:1 line. 
#    - When there are issues, we typically see the points in the tails further 
#      from the reference line than in the middle. 

# 3. Homogeneity of variances - We want the response variable to have ~ equal variance
#    across the range of values in our explanatory variable, or in different groups. 
#    - We want to see a similar spread of variance. 
#    - First, visualise the relationship between x and y variables. 
#    - Second, we must actually look at residuals, not raw data as in (1). 
#      So, we must look at the scale-location plot. 
#      We want to see a ~ horizontal red line and points equally scatted around the line. 

# To test any model assumptions, we first actually have to fit a linear model,
# and then we perform diagnostics on the model we have fit. 

################################################
# - Import data and check for import errors
################################################

# Load required packages 
library(tidyverse)
library(DHARMa)

# Import data
data <- readxl::read_excel("./data_raw/poisson_data.xlsx")

# Check data imported properly
str(data)
head(data)

# Make temperature a factor 
data <- data %>%
  mutate(temp = as.factor(temp))
data

#########################
# What is our hypothesis?
#########################

# - Larger females (adult_mass) will produce more offspring. 

########################################################
# - Run first linear model 
########################################################

mod1 <- lm(larvae ~ adult_mass,
           data = data)
summary(mod1)

# 1. Linearity assumption - we can use the residuals vs fitted plot 
#    - We want to see no pattern (the red line should be straight - horizontal). 
#    - This is indicative of a linear relationship between our explanatory variables
#    - and our response variable. 

plot(mod1, 1)

# Clearly we can see that there is no distinct pattern in our response depending
# on the different treatment groups, and that y is somewhat linearly related to x. 

# 2. Residuals follow a normal distribution 
#    - We want to see the points fall approximately along the reference 1:1 line. 
#    - When there are issues, we typically see the points in the tails further 
#      from the reference line than in the middle. 

plot(mod1, 2)

# Clearly, our residuals are not approximating the 1:1 line. 
# This means that our data does not follow a normal distribution. 

# 3. Homogeneity of variances - We want the response variable to have ~ equal variance
#    across the range of values in our explanatory variable, or in different groups. 
#    - We want to see a similar spread of variance. 
#    - First, visualise the relationship between x and y variables. 
#    - Second, we must actually look at residuals, not raw data as in (1). 
#      So, we must look at the scale-location plot. 
#      We want to see a ~ horizontal red line and points equally scatted around the line. 

# Visualise relationship between temp and larvae produced
ggplot(data = data, aes(x = temp,
                        y = larvae)) +
  geom_boxplot()

# Visualise relationship between adult body mass and larvae produced
ggplot(data = data, aes(x = adult_mass,
                        y = larvae)) +
  geom_point()

plot(mod1, 3)

# Our model shows evidence of variance being greater at some values than others.
# Note the 'step' in the red line around x = 20-25. 
# Fail to meet the assumption of homogeneity of variances. 

# A linear model is clearly inappropriate. 

sim_mod <- simulateResiduals(mod1)
plot(sim_mod)

#####################################
# - Generalised linear models (GLM)
#####################################

# The next step is to analyse our data with a generalised linear model (GLM). 
# The GLM is just an extension of linear models (e.g. linear regression, ANOVA). 
# The primary difference is that GLM's can model data that:
#     - Is not normally distributed 
#     - Does not follow a constant mean-variance relationship. 

# For example, we often want to model binary data (presence/absence) or
# or things like species abundance data (which often has many zeroes). 

# All we have to do is tell our model to expect data that follows a certain 
# distribution (e.g. a poisson distribution), instead of a normal distribution. 
# How does this work? (Very roughly and briefly)
# 1. Link function - GLM's require that we specify how the mean changes with
#    the predictor/independent variable.
# 2. Variance function - Function to describe how the variance changes with the mean. 

#######################################################################################
# What does a poisson distribution look like?
# DO NOT RUN CODE BELOW: THIS IS JUST TO ILLUSTRATE
reps <- 50000
nexps <- 5
rate <- 0.1
set.seed(0)
system.time(
  x1 <- replicate(reps, sum(rexp(n=nexps, rate=rate)))
) 

# Visualise simulated poisson data 
ggplot(data.frame(x1), aes(x1)) + 
  geom_histogram(aes(y=..density..)) +
  stat_function(fun=function(x)dgamma(x, shape=nexps, scale=1/rate),
                color="red", size=2)
#######################################################################################

#######################################
# - Run our first GLM - count data
#######################################

# Run a Poisson GLM
mod2 <- glm(larvae ~ adult_mass,
            data = data,
            family = poisson(link = "log"))
summary(mod2)

# Unfortunately, we cannot use the same methods to check the diagnostics of GLM's
# as we did for linear models. 
# - We are still going to produce the same or similar plots, we just need 
#   some different R code to do this. 

# 1. Our data points (y) come from a data distribution with a known
#    mean-variance relationship. 
library(mvabund)
mod2a <- manyglm(larvae ~ adult_mass,
                 data = data, 
                 family = "poisson")
plot(mod2a)

# Given that we can't see any massive fan shape or U shape in our residuals,
# we can conclude that the mean-variance relationship we specificied for our GLM
# (i.e. poisson - variance increases linearly with mean), was reasonable for our data.

# Let's check whether our model residuals are appropriate. 
mod2.res = simulateResiduals(mod2)
plot(mod2.res)

# The qqplot on the left shows that our residuals approximate the expectation under 
# the poisson distribution pretty well. 
# The KS test is a formal statistical test of our data distribution vs 
# the poisson distribution, a non-significant value (i.e. ns is good). 
# This means that there is no signficant difference in the distribution of our 
# residuals from a poisson distribution.

# 2. We need to look for potential overdispersion
#    - Remember that poisson models assume that mean and variance increase linearly
#    - We must confirm this by calculating an overdisperson statistics
#       - Overdispersion (ods > 2 below) indicates the model is not capturing the 
#         variance correctly. 
(ods <- mod2$deviance / mod2$df.residual)

# ods < 2, so overdispersion DOES NOT seem to be a problem here. 
# If you do get overdispersion, there are different GLM's to run. 
# - If you need to do this, come speak to me. Beyond scope of this course. 

# ods < 0.8 indicates underdispersion (data are less varible than expected under poisson
# distribution). 
# - There is little to be done if you have underdispersion. 
# - Underdispersion will mean for conservative p-vals and effect sizes. 
#   Not a bad thing. (Florian Hartig, pers. comm.)

# Here, we have a really good dispersion statistic (~1) - so our poisson model captures
# the variation around the data well. 

####################################
# - Interpreting model output 
####################################

# Let's extract the  model results that we are really interested in.
summary(mod2)

# Notice there is no tempa - R is comparing b, c and d to a here. 

###########
# P-values:
###########

# The significant P-value for tempd indicates that larvae produced by insects 
# from tempa and tempd are statistically different. 
# tempa, tempb and tempc are not statistically different. 

# Adult mass had no significant effect on no. of larvae produced 

###########
# Estimates:
###########

# The sign of the estimate columns gives an indication of the magnitude and 
# direction of the effect.
# For example, the positive sign of the estimate for 'tempd' = 0.98, 
# indicates that the number of larvae produced is higher in 
# treatment 'tempd' than 'tempa'



















#######################################################################
#######################################################################
#######################################################################
# - Section 2: Analysis of binary data (or categorical factors)
#######################################################################
#######################################################################
#######################################################################

# Let's return to our original dataset
head(data)

# The adults column is a binary variable, 1 = adults emerged, and 0 = no adults. 
# Let's try analyse this data with a linear model 

# Fit your linear model
mod1 <- lm(adults ~ temp * adult_mass,
           data = data)

# 1. Linearity assumption - we can use the residuals vs fitted plot 
#    - We want to see no pattern (the red line should be straight - horizontal). 
#    - This is indicative of a linear relationship between our explanatory variables
#    - and our response variable. 

plot(mod1, 1)

# Clearly we can see that there is a distinct pattern in our response depending
# on the different treatment groups.
# The red line is nowhere near straight. Not great. 

# 2. Residuals follow a normal distribution 
#    - We want to see the points fall approximately along the reference 1:1 line. 
#    - When there are issues, we typically see the points in the tails further 
#      from the reference line than in the middle. 

plot(mod1, 2)

# Residuals are okay. 

# 3. Homogeneity of variances - We want the response variable to have ~ equal variance
#    across the range of values in our explanatory variable, or in different groups. 
#    - We want to see a similar spread of variance. 
#    - First, visualise the relationship between x and y variables. 
#    - Second, we must actually look at residuals, not raw data as in (1). 
#      So, we must look at the scale-location plot. 
#      We want to see a ~ horizontal red line and points equally scatted around the line. 

# Visualise relationship between temp and adults emerged
ggplot(data = data, aes(x = temp,
                        y = adults)) +
  geom_jitter(width = 0.1, height = 0.1)

# Visualise relationship between adult body mass and adults emerged
ggplot(data = data, aes(x = adult_mass,
                        y = adults)) +
  geom_jitter(width = 0.1, height = 0.1)

plot(mod1, 3)

# Our model shows evidence of variance being greater at some values than others.
# # Fail to meet the assumption of homogeneity of variances. 

# A linear model is clearly inappropriate. 

#######################################
# - Run our second GLM - binary data
#######################################

# Run a binomial GLM
# This models tries to predict a yes/no, 1/0, left/right, based on the data we use. 

mod2 <- glm(adults ~ temp * adult_mass,
            data = data,
            family = binomial(link = "logit"))

# Unfortunately, we cannot use the same methods to check the diagnostics of GLM's
# as we did for linear models. 
# - We are still going to produce the same or similar plots, we just need 
#   some different R code to do this. 

# 1. Our data points (y) come from a data distribution with a known
#    mean-variance relationship. 
library(mvabund)
mod2a <- manyglm(adults ~ temp * adult_mass,
                 data = data, 
                 family = "binomial")
plot(mod2a)

# Given that we can't see any massive fan shape or U shape in our residuals,
# we can conclude that the mean-variance relationship we specificied for our GLM
# (i.e. poisson - variance increases linearly with mean), was reasonable for our data.

# Let's check whether our model residuals are appropriate. 
mod2.res = simulateResiduals(mod2)
plot(mod2.res)

# The qqplot on the left shows that our residuals approximate the expectation under 
# the binomial distribution pretty well. 
# The KS test is a formal statistical test of our data distribution vs 
# the binomial distribution, a non-significant value (i.e. ns is good). 
# This means that there is no signficant difference in the distribution of our 
# residuals from a binomial distribution.

# 2. There is a linear relationship between x and y. 

# Now, we plot our model against each covariate seperately 
# We want to see similar distributions (variances) and the 
# thick black line in the different groups close to res = 0.50
plotResiduals(data$temp,  
              mod2.res$scaledResiduals)
plotResiduals(data$adult_mass,  
              mod2.res$scaledResiduals)

# There is some little variation between x and y, approximately linear. 

plot(mod2.res)

# If we inspect the plot on the right, we can see that the red lines are not 100%
# matched up with the dashed lines, but they are not too dissimilar, so we 
# can probably just carry on with our GLM, as we can assume x and y are relatively
# linearly related. 

####################################
# - Interpreting model output 
####################################

# Let's extract the  model results that we are really interested in.
summary(mod2)

# Notice there is no tempa - R is comparing b, c and d to a here. 

###########
# P-values:
###########

# The significant P-value for tempd indicates that larvae produced by insects 
# from tempa and tempd are statistically different. 
# tempa, tempb and tempc are not statistically different. 

# Adult mass had no significant effect on no. of larvae produced 

###########
# Estimates:
###########

# The sign of the estimate columns gives an indication of the magnitude and 
# direction of the effect.
# For example, the positive sign of the estimate for 'tempd' = 0.98, 
# indicates that the number of larvae produced is higher in 
# treatment 'tempd' than 'tempa'