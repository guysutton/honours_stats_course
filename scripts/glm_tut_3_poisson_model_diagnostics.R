###########################################################
###########################################################
# - Introduction to general linear models (GLM)
# - Tutorial #3 - Model diagnostics
# - Honours Class Tutorial
# - Rhodes Universtiy
# - Script written: 17/02/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# Import data
data <- readxl::read_excel("./data_raw/poisson_data.xlsx")

# Check data imported properly
str(data)
head(data)

# We are trying to predict the number of larvae produced by
# female insects of a different mass 
mod2 <- glm(larvae ~ adult_mass,
            data = data,
            family = poisson(link = "log"))
summary(mod2)

####################
# Model diagnostics 
####################

# 1. Do our data fit the data distribution well? 
#    - To do this, we check our model residuals. 
#    - Residuals represent variation in the data that your model can't account for. 
#    - We use residual plots to discover patterns.
#    - We don't want to see any systematic patterns in residuals - should be a straight line
#    - We cannot use the standard lm diagnostics - deviance residuals not expected to be normal
#      for poisson regression. 
plot(mod2)[1] # NOT USEFUL FOR POISSON

# We need to use the DHARMa package
# Uses standardised residuals, irrespective of model type (poisson, binomial)
# So, we want to see a straight line to indicate a potentially good model fit 
# INSTALL LATEST VERSION OF DHARMA
#devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
#                         dependencies = T, build_vignettes = T)
library(DHARMa)
mod2.res = simulateResiduals(mod2)
testUniformity(mod2.res)

# The qqplot on the left shows that our residuals approximate the expectation under 
# the poisson distribution REALLY POORLY.  
# The KS test is a formal statistical test of our residual distribution vs 
# the poisson distribution, a non-significant value (i.e. ns is good). 

# Here, we have a significant (P < 0.05) KS test, so there is a significanct 
# difference between the distribution of our model residuals and the 
# assumed distribution of poisson errors. 
# - We want to see all those black triangles in the QQ plot falling
#   approximately on the red 1:1 line, and a KS-test P-value > 0.05. 

# 2. Linearity of expected value of Y and predictors (x)
# - Another assumtpion fo GLM's is linear relationship between
#   expected value of y and predictors (x). 
# - We expect no trend in the residuals - any trend is a concern.
# - To do this, we must inspect residuals vs fitted plot (right hand side)
plot(mod2.res)
plotResiduals(mod2.res)

# We want to see the bold black lines around the 0.50 line.
# We see large deviations from that here.
# - Indicates that our model is displaying heterogeneity
# - This means the variation in y is dependent on some other factor in the model. 
# - This is NOT GOOD - indicates a poorly fit model. 

# 3. We can test formally for potential overdispersion
#    - Remember that poisson models assume that mean and variance increase linearly
#    - We must confirm this by calculating an overdisperson statistics
#       - Overdispersion (ods > 2) indicates the model is not capturing the 
#         variance correctly. 
#       - ods between 0.8 and 2 indicates model captures disperson properly
#       - ods < 0.8 indicates underdispersion (data are less varible than expected under poisson
#         distribution). 
#          - There is little to be done if you have underdispersion. 
#          - Underdispersion will mean for conservative p-vals and effect sizes. 
#          - Not a bad thing. (Florian Hartig, pers. comm.)
(ods <- mod2$deviance / mod2$df.residual)

# Here, ods > 2, so overdispersion is a REAL problem here. 
# If you do get overdispersion, there are different GLM's to run. 
# - If you need to do this, come speak to me. 
# - Beyond scope of this course.  
