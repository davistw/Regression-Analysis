
# Clears environment

rm(list=ls())

gc()



##### Taste Analysis #####

# Load needed libraries

library(faraway)

# Load and check data

data(cheddar)
head(cheddar)

# Scatter Plots for Predictors

attach(cheddar)
plot(Acetic, taste)
plot(H2S, taste)
plot(Lactic, taste)

# Correlations

cor(Acetic, taste)
cor(H2S, taste)
cor(Lactic, taste)

# Multiple Regression Model

model <- lm(taste ~ Acetic + H2S + Lactic, cheddar)
summary(model)

# Plots to assess assumptions

# Residual and QQ Plots

plot(model)

# Apply .01 increase to H2S Predictor Values

cheddar$H2S <- cheddar$H2S + .01

model1 <- lm(taste ~ Acetic + H2S + Lactic, cheddar)
summary(model1)

# Apply Log Scale
# We'll assume that Acetic = 1 for the percent calculation

percent_calculation <- (exp(1 + 0.01)-exp(1))/exp(1)
print(percent_calculation)

# Confidence Intervals

confint(model, H2S, level=0.90)
confint(model, H2S, level=0.95)

