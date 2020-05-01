###########################################################
###########################################################
# - Introduction to general linear models (GLM)
# - Tutorial #2 - Poisson GLM's
# - Honours Class Tutorial
# - Rhodes Universtiy
# - Script written: 12/02/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

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
# What does a poisson distribution look like vs normal distribution?
n <- 1000
set.seed(42)
x1 <- rpois(n, lambda = 1)
x10 <- rpois(n, lambda = 10)

par(mfrow=c(1,2))
hist(x1, xlim = c(0, 25), seq(0, 25, by = 1),
     main = "poisson")
hist(x10, xlim = c(0, 25), seq(0, 25, by = 1),
     main = "normal")
#######################################################################################

# Run same simulated data from tutorial #1
raw_data <- tibble(
  temp = c(11.9, 14.2, 15.2, 16.4, 17.2, 18.1, 
           18.5, 19.4, 22.1, 22.6, 23.4, 25.1),
  abundance = c(185L, 215L, 332L, 325L, 408L, 421L, 
                406L, 412L, 522L, 445L, 544L, 614L))
head(raw_data)

# Visualise relationship between temperature and abundance
ggplot(data = raw_data, aes(x = temp,
                            y = abundance)) + 
  geom_point()

# Run poisson GLM
mod1 <- glm(abundance ~ temp, 
            data = raw_data,
            family = poisson(link = "log"))

# family = poisson is the variance function. 
#        = This tells R how the variance of response variable changes with mean of x.
#        = poisson distribution has variance increasing with the mean. 

# link = "log" = link function.
#              = how does expected value of y change with x? 
# NB - Not the same as a log transformation of y. 
#    - log transformation changes y, but the model errors are still assumed
#      to be normal
#    - link function changes expected value of y, with model errors following a 
#      poisson distribution. 
#    - This is a tricky concept. 

# Print summary of model output
summary(mod1)

# How do we interpret this output?
# - (1) Intercept estimate - value of abundance where X (temp) = 0
#       This value is on the log-scale, so we need to back-transform to get counts.
#       coef(model_name) extracts the intercept value for you
exp(coef(mod1))

#       Intercept = 94.04 abundance where temp = 0. 
#       Ask yourself, is that reasonable? 
#       In this case, probably yes. 

# - (2) Is my treatment/factor significant?
#       Look at the row for your predictor variable (here: temp)
#       Look at the P-value: P <0.05 means this variable is significant 
#       - Some type of association between temperature and abundance. 

# - (3) How does my treatment effect response?
#       Look at the estimate value in your predictor variable row
#       > 0 estimate = response increases with greater values of predictor
#       < 0 estimate = response decreases with greater values of predictor
#       NB - This value is on log scale - need to back-transform
exp(coef(mod1)[2])
exp(0.075595)

#      For every 1 unit increase in temperature, there is a 7% increase in abundance. 

# Plot model predictions 
temps <- seq(0, 35, 1)
mod_predict <- predict(mod1, 
                       list(temp = temps), 
                       type="response")
mod_predict <- as.data.frame(mod_predict)
mod_predict <- mod_predict %>%
  mutate(pred = mod_predict,
         t = seq(0:35))

# Plot your model predictions
ggplot(data = raw_data, aes(x = temp,
                            y = abundance)) + 
  geom_point() + 
  geom_line(data = mod_predict, aes(x = t, 
                                    y = pred)) +
  scale_y_continuous(breaks = seq(0, 1500, 250),
                    limits = c(0, 1500)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic()


