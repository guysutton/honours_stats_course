###########################################################
###########################################################
# - Introduction to general linear models (GLM)
# - Tutorial #4 - Plot poisson glm
# - Honours Class Tutorial
# - Rhodes Universtiy
# - Script written: 23/02/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# Load required libraries
library(tidyverse)

# Set plot theme
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

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

## some data to predict at: 100 values over the range of adult_mass
ndata <- with(data, 
              data_frame(adult_mass = seq(min(adult_mass)-5, 
                                          max(adult_mass)+10,
                                          length = 50)))

## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, 
                    fit = predict(mod2, 
                                  newdata = ndata, 
                                  type = 'response'))

## plot it
plt <- ggplot(ndata, aes(x = adult_mass, y = fit)) +
  geom_line() +
  #geom_rug(aes(y = visited, colour = lvisited), data = ndata) +
  scale_colour_discrete(name = 'Visited') +
  labs(x = 'Adult mass (g)', y = 'No. of larvae produced')
plt

# Extract inverse link from the model
ilink <- family(mod2)$linkinv

# Add predictions from the model to a new df
ndata <- bind_cols(ndata, setNames(as_tibble(predict(mod2, 
                                                     ndata,
                                                     se.fit = TRUE)[1:2]),
                                  c("fit_link", "se_link")))
head(ndata)

# Create the confidence interval and backtransform
ndata <- ndata %>%
  mutate(fit_resp = ilink(fit_link),
         right_upr = ilink(fit_link + (2 * se_link)),
         right_lwr = ilink(fit_link - (2 * se_link)))
head(ndata)

# Make the plot
ggplot(data = ndata) +
  geom_line(aes(x = adult_mass,
                y = fit)) +
  geom_ribbon(aes(x = adult_mass,
                  ymin = right_lwr,
                  ymax = right_upr),
              alpha = 0.2) +
  labs(x = "Adult mass (g)",
       y = "No. of larvae produced") +
  scale_x_continuous(breaks = seq(0, 15, 3),
                     limits = c(0, 14)) +
  scale_y_continuous(breaks = seq(0, 100, 10), 
                     limits = c(0, 100))
