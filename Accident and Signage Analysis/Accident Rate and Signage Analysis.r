
# Clears environment

rm(list=ls())

gc()




################### Accident Rate versus Signage Analysis ####################

# Load data file and check first six entries.
data = read.csv("Highway1.csv", head = TRUE, sep = ",")
head(data)


# Assign variables of interest to vectors.
rate = as.numeric(data[,2])
signs = as.numeric(data[,6])


##### Exploratory analysis #####


# Scatter Plot
plot_scatter <- plot(signs,rate)


# Correlation Coefficient
corr_coef <- cor(signs,rate)
corr_coef

##### Modeling #####

# Create Linear Regression Model
model = lm(rate ~ signs)
summary(model)


# Confidence Interval
confint(model, level=0.95)


##### Plots #####

# Scatter Plot
plot_scatter <- plot(signs,rate)


# Residual and QQ Plots
model1 = lm(rate ~ signs)
plot(model1)


# Prediction For New Point and Prediction Interval
new_point = data.frame(signs = 1.25)
predict(model,new_point,interval=c("prediction"))





