# Clears environment

rm(list=ls())

gc()

##### Titanic Analysis #####

# Load and Check Data

data <- read.csv("titanic.csv",head=TRUE,sep=",")
head(data)


# Box Plots

attach(data)
boxplot(Survived ~ Age, xlab = "Age", ylab = "Survived")
boxplot(Survived ~ Fare, xlab = "Fare", ylab = "Survived")

# Predictor Median/Mean Summaries

summary(data[data$Survived==1,"Age"])
summary(data[data$Survived==1,"Fare"])

# Sib_sp and Par_ch Conversions

data$Sib_sp <- ifelse(data$Sib_sp >=4,"above_4",data$Sib_sp)
data$Sib_sp <- as.factor(data$Sib_sp)


data$Par_ch <- ifelse(data$Par_ch >=4,"above_4",data$Par_ch)
data$Par_ch <- as.factor(data$Par_ch)


# Bar-Charts

library(plyr)
library(ggplot2)

# Passenger Class
ggplot(ddply(data,.(Pclass),summarise, rr=100*sum(Survived)/length(Survived)), 
       aes(x=Pclass,y=rr))+geom_bar(stat = "identity",width=0.5)+ labs(x="Passenger Class", y="% Response Rate")

# Sex
ggplot(ddply(data,.(Sex),summarise, rr=100*sum(Survived)/length(Survived)), 
       aes(x=Sex,y=rr))+geom_bar(stat = "identity",width=0.5)+ labs(x="Sex", y="% Response Rate")


# Sib_sp
ggplot(ddply(data,.(Sib_sp),summarise, rr=100*sum(Survived)/length(Survived)), 
       aes(x=Sib_sp,y=rr))+geom_bar(stat = "identity",width=0.5)+ labs(x="siblings/spouses of the passenger", y="% Response Rate")


# Par_ch
ggplot(ddply(data,.(Par_ch),summarise, rr=100*sum(Survived)/length(Survived)), 
       aes(x=Par_ch,y=rr))+geom_bar(stat = "identity",width=0.5)+ labs(x="parents/children of the passenger", y="% Response Rate")


# Conversion of Pclass to Factor Variable (Sib_sp was already converted in earlier code)

data$Pclass <- as.factor(data$Pclass)


# Create Logistic Regression Model 

model1 <- glm(Survived ~ Pclass + Sex + Age + Sib_sp, family = binomial, data)
summary(model1)

# Coefficient Interpretation

exp(-1.360179)
exp(-2.497465)
exp(-2.710295)

# Confidence Interval

confint(model1, level = 0.95)

# Aggregate Data and Build New Model

attach(data)
data.n = aggregate(Survived ~ Pclass + Sex + Sib_sp, FUN=length)
data.y = aggregate(Survived ~ Pclass + Sex + Sib_sp, FUN=sum)
data.agg = data.frame(Survived = data.y$Survived,
                      Total = data.n$Survived, Pclass = data.n$Pclass, 
                      Sex = data.n$Sex, Sib_sp = data.n$Sib_sp) 

model2 <- glm(cbind(Survived, Total-Survived) ~ Pclass + Sex + Sib_sp, 
              data = data.agg, family=binomial)
summary(model2)

# Test Goodness of Fit

model2$df.residual

c((deviance(model2)), 1-pchisq(deviance(model2),17))

# Plots

residuals.model <- resid(model2,type="deviance")

boxplot(residuals.model ~ data.n$Pclass,ylab = "Residuals", xlab = "Pclass")
boxplot(residuals.model ~ data.n$Sex,ylab = "Residuals", xlab = "Sex")
qqnorm(residuals.model, ylab="Residuals")
qqline(residuals.model,col="red",lwd=2)
hist(residuals.model,10,xlab="Residuals", main="Histogram: Residuals")


# Predictions

model3 <- glm(Survived ~ Pclass + Sex + Age + Sib_sp, family = binomial, data)

newdata1 <- data.frame(Pclass = "1", Sex = "female", Age = 20, Sib_sp = "1")
predict(model3, newdata1, type=c("response"))

newdata2 <- data.frame(Pclass = "3", Sex = "male", Age = 21, Sib_sp = "above_4")
predict(model3, newdata2, type=c("response"))












