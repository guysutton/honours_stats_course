###########################################################
###########################################################
# - Introduction to general linear models (GLM)
# - Tutorial #5 - Write-up of results from model
# - Honours Class Tutorial
# - Rhodes Universtiy
# - Script written: 27/02/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# Load required libraries
library(tidyverse)

# Import data
data <- readxl::read_excel("./data_raw/poisson_data.xlsx")

# Check data imported properly
str(data)
head(data)

# Remember, we were trying to predict the number of larvae produced by
# female insects of a different mass 
mod2 <- glm(larvae ~ adult_mass,
            data = data,
            family = poisson(link = "log"))
summary(mod2)

#################################
### How to write-up your results:
#################################

# (1) Is your variable significant or not?
#     - Look at the summary call, e.g. summary(mod_name): 
#       look at Pr column
summary(mod2)

# Here, our predictor variable is significant (P < 0.05). 

# (2) How does the variable (adult_mass) influence the outcome (no_larvae)?
#     - Look at the summary call, e.g. summary(mod_name):
#       look at the estimate column
summary(mod2)

# If estimate > 0, this means as predictor increases, response increases.
#                  - e.g. as adult_mass increases, more larvae are produced.
# If estimate < 0, this means as predictor increases, response decreases.
#                  - e.g. as adult_mass increases, fewer larvae are produced. 

# Here, our estimate (0.165) is > 0, so we can conclude that as 
# adult_mass increases, so does the number of larvae they produce. 

# *** TIP: Plot your model predictions to make sure you have interpreted your
#          estimate properly. (see previous tutorial video for plot). 

# (3) Extract parameter estimates 
#     - As per (2), parameter estimate indicates the direction and magnitude 
#       of the effect of your predictor on the response variable. 
#     - We must extract this estimate from the model
#     - Because this is poisson, we need to exponentiate the value so that
#       it is interpretable (the reported value is on the link function scale).
#     - Code below exponentiates (exp), the coefficient (coef), from our glm
#       model (mod2), and we are extracting the second value [[2]]. 
#     - If we had another factor (e.g. temperature) in the row below adult_mass
#       in the summary call, we would use [[3]]. 
exp(coef(mod2)[[2]])

# Here, our parameter estimate is 1.18.
# Remember from our previous videos, that this means:
# - For every 1 unit increase in adult mass (i.e. as adults become 1g heavier),
#   they will produce approximately 18% more larvae. 

# (4) Extract parameter uncertainty 
#     - It is always good to report the uncertainty of your model estimates.
#     - To do this, we report a 95% confidence interval.
#       - The interval DOES NOT say that we can be 95% certain that 
#         our estimate falls somewhere within this range (lots of papers do this 
#         incorrectly.)
#     - The interval shows that if we had to repeat this experiment 100 times,
#       we would expect our parameter to fall within the 95% CI, 95 times.
#     - A subtle but very inportant distinction - outside the scope of this course. 
#     - We will now extract the 95% CI for our adult_mass estimate
exp(confint(mod2))

# Here, our 95% CI for adult body mass is 1.14 - 1.22
# This means that there is some uncertaintly in our parameter estimate from (3).
# Interpretation: For every 1 unit increase in adult mass (i.e. as adults 
# become 1g heavier), we can be quite confident that they will produce 
# 14% t0 22% more larvae (slightly different from the estimate in (3) of 18%). 

###############################
# How would you write-this up?:
###############################

# Everyone has a different style of writing. 
# Below I give some examples. 
# Any text within [] is me explaining, and not actually included in the text. 
# This is my approach, it does not have to be yours. 

# (1) The standard example is:
# Adult body mass had a significant influence on the number of larvae produced 
# (P < 0.05)

# - In my opinion, this is pretty poor. 
# - It really tells us nothing. 

# (2) Improvement on (1):
# Adult body mass had a significant influence on the number of larvae produced 
# (beta = 1.18; P < 0.05). [where beta is estimate from 3]. 

# - Adding the parameter estimate at least tells the reader the sign
#   and magnitude of the effect adult_mass has on larvae. 

# (3) More improvements:
# Adult body mass had a significant influence on the number of larvae produced 
# (beta = 1.18; 95% CI = 1.14-1.22; P < 0.05).

# - Adding the CI shows the uncertainty in our estimate.
# - Much better. 
# - We can still do better. 

# (4) Actually explain your results.
#     - This is my prefered approach. 

# Larger adults produced more larvae than smaller adults (P < 0.05). 
# For every 1g increase in adult body mass, adults produced approximately
# 18% more larvae (95% CI: 1.14 - 1.22) [could report this as 14 - 22%, 
# probably better as %'s anyway] (Fig. x) [refer the reader to your plot
# of the model prediction]. 

# - We have told the reader exactly how the predictor effects response,
#   we have reported uncertainty, and referred the reader to our awesome graph!!! 

# Your results for your paper are now ready to go!!!




