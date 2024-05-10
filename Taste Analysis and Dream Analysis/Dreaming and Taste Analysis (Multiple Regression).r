
# Clears environment

rm(list=ls())

gc()



##### Dreaming and Taste Analysis (Multiple Regression) #####

# Load and check data

data = read.csv("sleep.csv", header=TRUE, sep = ",")

head(data)

# Response Variables

NonDreaming <- data$NonDreaming

Dreaming <- data$Dreaming

TotalSleep <- data$TotalSleep

# Continuous Variables

BodyWt <- data$BodyWt

BrainWt <- data$BrainWt

LifeSpan <- data$LifeSpan

Gestation <- data$Gestation

# Categorical Variables

Predation <- data$Predation

Exposure <- data$Exposure

Danger <- data$Danger


# Scatter Plots for Predictors (NonDreaming)

plot(BodyWt, NonDreaming)
plot(BrainWt, NonDreaming)
plot(LifeSpan, NonDreaming)
plot(Gestation, NonDreaming)

# Correlations (NonDreaming)

NonDreamingcor <- cor(data[c(2,3,7,8)],data[4])

NonDreamingcor


# Inspection and Transformation (NonDreaming)

plot(BodyWt, NonDreaming) 
abline(lm(NonDreaming ~ BodyWt, data = data))
plot(BrainWt, NonDreaming) 
abline(lm(NonDreaming ~ BrainWt, data = data))
plot(LifeSpan, NonDreaming) 
abline(lm(NonDreaming ~ LifeSpan, data = data))
plot(Gestation, NonDreaming) 
abline(lm(NonDreaming ~ Gestation, data = data))

plot(log(BodyWt), NonDreaming)
abline(lm(NonDreaming ~log(BodyWt), data = data))
plot(log(BrainWt), NonDreaming)
abline(lm(NonDreaming ~log(BrainWt), data = data))
plot(log(LifeSpan), NonDreaming)
abline(lm(NonDreaming ~log(LifeSpan), data = data))
plot(log(Gestation), NonDreaming)
abline(lm(NonDreaming ~log(Gestation), data = data))

# BoxPlots (NonDreaming)

boxplot(NonDreaming ~ Predation, xlab = "Predation", ylab = "NonDreamy Sleep")
boxplot(NonDreaming ~ Exposure, xlab = "Exposure", ylab = "NonDreamy Sleep")
boxplot(NonDreaming ~ Danger, xlab = "Total Danger", ylab = "NonDreamy Sleep")


# Scatter Plots for Predictors (Dreaming)

plot(BodyWt, Dreaming)
plot(BrainWt, Dreaming)
plot(LifeSpan, Dreaming)
plot(Gestation, Dreaming)

# Correlations (Dreaming)

cor(BodyWt, Dreaming)
cor(BrainWt, Dreaming)
cor(LifeSpan, Dreaming)
cor(Gestation, Dreaming)

# Inspection and Transformation (Dreaming)

plot(BodyWt, Dreaming) 
abline(lm(Dreaming ~ BodyWt, data = data))
plot(BrainWt, Dreaming) 
abline(lm(Dreaming ~ BrainWt, data = data))
plot(LifeSpan, Dreaming) 
abline(lm(Dreaming ~ LifeSpan, data = data))
plot(Gestation, Dreaming) 
abline(lm(Dreaming ~ Gestation, data = data))

plot(log(BodyWt), Dreaming)
abline(lm(Dreaming ~log(BodyWt), data = data))
plot(log(BrainWt), Dreaming)
abline(lm(Dreaming ~log(BrainWt), data = data))
plot(log(LifeSpan), Dreaming)
abline(lm(Dreaming ~log(LifeSpan), data = data))
plot(log(Gestation), Dreaming)
abline(lm(Dreaming ~log(Gestation), data = data))

# BoxPlots (Dreaming)

boxplot(Dreaming ~ Predation, xlab = "Predation", ylab = "Dreamy Sleep")
boxplot(Dreaming ~ Exposure, xlab = "Exposure", ylab = "Dreamy Sleep")
boxplot(Dreaming ~ Danger, xlab = "Total Danger", ylab = "Dreamy Sleep")

# Model 3

model3 <- lm(NonDreaming ~ BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger, data = data)

summary(model3)

plot(model3, cook.levels = c(4/42,0.5,1))

# Model 3a

model3a <- lm(log(NonDreaming) ~ BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger, data = data)

summary(model3a)

plot(model3a, cook.levels = c(4/42,0.5,1))

# Model 3b

model3b <- lm(NonDreaming ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger, data = data)

summary(model3b)

# Model 3c

model3c <- lm(NonDreaming ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Danger, data = data)

summary(model3c)

# Model 3d

finalmodel <- lm(log(NonDreaming) ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Danger, data = data)

summary(finalmodel)

# Remove Echidnas

data2 = data[-11, ]

# Model 4

model4 <- lm(Dreaming ~ BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger, data = data2)

summary(model4)

plot(model4, cook.levels = c(4/42,0.5,1))

# Model 4a

model4a <- lm(log(Dreaming) ~ BodyWt + BrainWt + LifeSpan + Gestation + Predation + Exposure + Danger, data = data2)

summary(model4a)

plot(model4a, cook.levels = c(4/42,0.5,1))

# Model 4b

model4b <- lm(Dreaming ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Predation + Exposure + Danger, data = data2)

summary(model4b)

# Model 4c

model4c <- lm(Dreaming ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Danger, data = data2)

summary(model4c)

# Model 4final

finalmodel4 <- lm(log(Dreaming) ~ log(BodyWt) + log(BrainWt) + log(LifeSpan) + log(Gestation) + Danger, data = data2)

summary(finalmodel4)

# Check assumptions for Final Models

plot(finalmodel, cook.levels = c(4/42,0.5,1))

plot(finalmodel4, cook.levels = c(4/42,0.5,1))


######################################################################################

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

