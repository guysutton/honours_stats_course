# Analysis of Anoliz lizard data for Honours class assignment 

# Load required packages
library(tidyverse)
library(DHARMa)

# Load data 
data <- readxl::read_excel("./data_raw/anolis.xlsx")

# Check data imported properly
str(data)
head(data)

# Reformat data 
data <- data %>%
  mutate(Species = as.factor(Species),
         Ecomorph = as.factor(Ecomorph),
         Island = as.factor(Island),
         Geog = as.factor(Geog))
str(data)
head(data)

#####################################################################
# Q2.a - Relationship between hostility and (i) brightness of dewlap 
#        and (ii) percentage cover of habitat in the environment
#####################################################################

# Does hostility correlate with brightness of the dewlap? 
ggplot(data = data, aes(x = Colour, y = Hostility)) +
  geom_point() +
  geom_smooth()

# - It certainly appears so. 

# Does hostility correlate with percentage cover of vegetation? 
ggplot(data = data, aes(x = Perc_cov, y = Hostility)) +
  geom_point() +
  geom_smooth()

# - Looks like it.  

# Run linear regression model. 
host1 <- lm(Hostility ~ Colour + Perc_cov, data = data)
plot(host1)

# - Linear model diagnostics looks okay. 

# Produce model summary
summary(host1)

# - Yes, there is a significant relationship between hostility and brightness of
#   the dewlap colour (Colour P < 0.05). 
#   - Hostility decreases with increasing dewlap colour brightness 

# - Yes, there is a significant relationship between hostility and percentage 
#   cover of vegetation (Perc_cov P < 0.05). 
#   - Hostility increases with increasing percentage cover of vegetation.  

###
# Q2b - Plot relationships from Q2a
###






###
# Q3 - Does hostility differ between eastern vs western islands? 
###

# Does hostility correlated with island? 
ggplot(data = data, aes(x = Geog, y = Hostility)) +
  geom_boxplot() 

# It doesn't appear so, but more variation in hostility on eastern island.  

# Let's test this with a t-test?
t.test(Hostility ~ Geog, 
       data = data, 
       var.equal = F)

# Check model diagnostics 
resid1 <- simulateResiduals(host1)
plot(resid1)

raw_data <- data %>%
  mutate(del = floor(DEL))
mod1 <- lm(del ~ Perc_cov, data = raw_data)
plot(mod1)

mod2 <- glm(del ~ SVL, 
            data = raw_data,
            family = poisson(link = "log"))
plot(mod2)

resid1 <- simulateResiduals(mod2)
plot(resid1)

###########################################################
###########################################################
###########################################################

# Load data 
data <- readxl::read_excel("./data_raw/poisson_data.xlsx")

# Check data imported properly
str(data)
head(data)

data <- data %>%
  mutate(temp = as.factor(temp),
         adults = as.factor(adults))
data

########################################################
# - Run first linear model and perform diagnostics 
########################################################

# Let's look at no. of ramets vs insect and nutrient treatments
mod1 <- lm(larvae ~ temp * adult_mass,
           data = data)

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

#############################
# - Assumptions of GLM
#############################

# 1. Our data points (y) come from a data distribution with a known
#    mean-variance relationship. 
#    - For example, a poisson model assumes that variance increases linearly 
#      with the mean. 
#    - To check this assumption, we plot the fitted vs residual plot again. 
#      We want to make sure there is no pattern (e.g. no fan shape). 

# 2. There is a linear relationship between x and y. 
#    - We want to see no pattern (the red line should be straight - horizontal). 
#    - This is indicative of a linear relationship between our explanatory variables
#    - and our response variable. 

#######################################
# - Run our first GLM - count data
#######################################

# Run a Poisson GLM
mod2 <- glm(larvae ~ temp * adult_mass,
            data = data,
            family = poisson(link = "log"))

# Unfortunately, we cannot use the same methods to check the diagnostics of GLM's
# as we did for linear models. 
# - We are still going to produce the same or similar plots, we just need 
#   some different R code to do this. 

# 1. Our data points (y) come from a data distribution with a known
#    mean-variance relationship. 
library(mvabund)
mod2a <- manyglm(larvae ~ temp * adult_mass,
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

# 2. There is a linear relationship between x and y. 

# Now, we plot our model against each covariate seperately 
# We want to see similar distributions (variances) and the 
# thick black line in the different groups close to res = 0.50
plotResiduals(data$temp,  
              mod2.res$scaledResiduals)
plotResiduals(data$adult_mass,  
              mod2.res$scaledResiduals)

# There is some variation between x and y. 

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



