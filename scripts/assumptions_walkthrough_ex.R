# Load libs
library("tidyverse")
library("ds4ling")
library("patchwork")
library("gvlma")

# Fit some models
## intercept-only model gives you the mean
mod0 <- lm(mpg ~ 1, data = mtcars)
summary(mod0) ## notice that the intercept == mean
## you can start with an intercept-only model and then build up from it

mod1 <- lm(mpg ~ wt, data = mtcars)
mod2 <- lm(dist ~ speed, data = cars[1:20, ])

plot_1 <- mtcars %>%
  ggplot() +
  aes(wt,mpg) +
  geom_point() +
  geom_smooth(method = lm)
plot_1

plot_2 <- cars[1:20, ] %>%
  ggplot() +
  aes(x=speed,y=dist) +
  geom_point() +
  geom_smooth(method = lm,fullrange=T) +
  coord_cartesian(xlim=c(0,15))
plot_2

## put two ggplots side by side
plot_1+plot_2

# Assumptions

# 1.
# The regression model is linear in parameters
# Eyeball it
plot_1 + plot_2


# 2. 
# The mean of residuals is zero
# How to check?: Check model summary and test manually
mean(mod1$residuals)
mean(mod2$residuals)


# 3.
# Homoscedasticity of residuals or equal variance
# How to check? 
# What are you looking for? The line should be more or less flat

## found in ds4ling package
diagnosis(mod1)



# 4.
# No autocorrelation of residuals (important for time series data)
# When the residuals are autocorrelated, it means that the current value 
# is dependent of the previous values and that there is an unexplained 
# pattern in the Y variable that shows up

#
# How to check? 2 methods
#

# 4a. afc plot
acf(mod1$residuals)   # visual inspection
data(economics)       # bad example
bad_auto <- lm(pce ~ pop, data = economics)
summary(bad_auto)
acf(bad_auto$residuals)  # highly autocorrelated from the picture.

# 4c. Durbin-Watson test
# lmtest::dwtest(mod1)
# lmtest::dwtest(bad_auto)

#
# How to fix it?
#

# One option: Add lag1 as predictor and refit model
# econ_data  <- data.frame(economics, resid_bad_auto = bad_auto$residuals)
# econ_data1 <- slide(econ_data, Var = "resid_bad_auto", NewVar = "lag1", slideBy = -1)
# econ_data2 <- na.omit(econ_data1)
# bad_auto2  <- lm(pce ~ pop + lag1, data = econ_data2)

# acf(bad_auto2$residuals)
# lawstat::runs.test(bad_auto2$residuals)
# lmtest::dwtest(bad_auto2)
# summary(bad_auto2)

#
# What happened? Adding the lag variable removes the autocorrelation so now 
# we can interpret the parameter of interest.
#
# (you might never do this)
#



# 5. predictor and residuals are not correlated
# How to check? cor.test
cor.test(mtcars$wt, mod1$residuals)



# 6. 
# Normality of residuals
# (increasingly bad)
autoplot(mod1, which = 2)
autoplot(mod2, which = 2)
autoplot(bad_auto, which = 2)






#
# You can check some assumptions automatically
#
gvlma::gvlma(mod1)
gvlma::gvlma(mod2)
gvlma::gvlma(bad_auto)




# 0. Create project 'diagnostics'
# 1. add folders "slides", "scripts", "data"
# 2. save mtcars to "data" (write_csv)
# 3. load "mtcars" (read_csv)
# 4. walk through diagnostics in Rscript
# 5. create ioslides
#   - sections
#   - lists
#   - bold, italics
#   - r chunks
# 6. add diagnostics to slides 
# 7. convert to slidify 
# 8. create repo, push, github pages
# install xaringan

# create a csv from a df
## write_csv(mtcars, file = "./data/untidy/mtcars_raw.csv")

#read & save csv as object
cars_temp <- read_csv(file="./data/untidy/mtcars_raw.csv")
cars_temp %>% 
  group_by(cyl) %>%
  summarize(avg = mean(mpg),sd=sd(mpg)) %>%
  arrange(sd) %>%
  write_csv(file = "./data/tidy/mtcars_tidy")

read_csv(file="./data/tidy/mtcars_tidy")

install.packages("here")
library(here)
here()

cars_temp %>% 
  group_by(cyl) %>%
  summarize(avg = mean(mpg),sd=sd(mpg)) %>%
  arrange(sd) %>%
  write_csv(
    file = here("data","tidy","mtcars_tidy.csv")
  )

read_csv(
  here("data","tidy","mtcars_tidy.csv")
)
## here() is better for reproducibility because people not using rstudio
## will be able to run the code without having to change paths

# install.packages("xaringan")







