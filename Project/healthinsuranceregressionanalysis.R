---
title: "HealthInsurance"
author: "Vanessa Trujillo"
date: "3/15/2019"
output:
  word_document: default
  html_document: default
---

#Importing Data
```{r setup, include=TRUE}
#Importing the data that we will analyze
library(readr)
#Saving all the imported data into a vector-base-fraame
P1_Dataset2F <- read_csv("P1_Dataset2F.csv")
```
#Cleaning data & Organizing Data
``` {r, include=TRUE}
#Setting imported data as a data-frame
str(P1_Dataset2F)
summary(P1_Dataset2F)
#Retrieving and gathering missing values from data-frame
missingValues <- which(is.na(P1_Dataset2F), arr.ind=TRUE)
# Removing all the missing values from the dataframe
newdat <- na.omit(P1_Dataset2F)
str(newdat)
summary(newdat)
#NOTE: We are not going to use ID in any of our analysis
#Turning each column of the data-frame into a vector with it's corresponding names
ID <- newdat$ID
Cost <- newdat$Cost
Age <- newdat$Age
Gender <- newdat$Gender
Interventions <- newdat$Interventions
Drugs<- newdat$Drugs
Complications <- newdat$Complications
Duration <- newdat$Duration
```
#boxplot statistics
```{r, include=TRUE}
#Stats: a vector of length 5, containing the extreme of the lower whisker, the lower ‘hinge’, the median, the upper ‘hinge’ and the extreme of the upper whisker.
#n: the number of non-NA observations in the sample.
#conf: the lower and upper extremes of the ‘notch’ 
#out:the values of any data points which lie beyond the extremes of the whiskers 
boxplot.stats(Cost)
boxplot.stats(Age)
boxplot.stats(Gender)
boxplot.stats(Interventions)
boxplot.stats(Drugs)
boxplot.stats(Complications)
boxplot.stats(Duration)

sd(Cost)
sd(Age)
sd(Gender)
sd(Interventions)
sd(Drugs)
sd(Duration)
```
#Starting analysis
#Analysis for entire multi-linear regression with First Model
```{r, include=TRUE}
library(car)
library(ggplot2)
modelOne <- lm(Cost~Age+Duration+Gender+Complications+Drugs+Interventions, data = newdat)
resOne <- resid(modelOne)
fittedMod <- fitted(modelOne)

qqnorm(resOne)
qqline(resOne)
shapiro.test(resOne)
vif(modelOne)
par(mfrow=c(2,2))
plot(modelOne)

#plotting with residuals and fitted model
ggplot(data = newdat,aes(resOne,Age))+labs(x="ei",y="Age")+ggtitle("ei vs X")+geom_point()+geom_smooth(method = "lm")
ggplot(data = newdat,aes(fittedMod,resOne))+labs(x="ei",y="yhat")+ggtitle("Ei vs Yhat")+geom_point()+geom_smooth(method = "lm")
shapiro.test(resOne) 

summary((modelOne))
anova(modelOne) # provides ANOVA table for overall model


# Analysis for this First Regression Model
ggplot(data = newdat,aes(Age,Cost))+labs(x="Age",y="Cost")+ggtitle("Y vs X")+geom_point()+geom_smooth(method = "lm")
ggplot(data = newdat,aes(Drugs,Cost))+labs(x="Drugs",y="Cost")+ggtitle("Y vs X")+geom_point()+geom_smooth(method = "lm")
ggplot(data = newdat,aes(Duration,Cost))+labs(x="Duration",y="Cost")+ggtitle("Y vs X")+geom_point()+geom_smooth(method = "lm")
ggplot(data = newdat,aes(Interventions,Cost))+labs(x="Interventions",y="Cost")+ggtitle("Y vs X")+geom_point()+geom_smooth(method = "lm")

# They show only 1's and 0's (boolean)
ggplot(data = newdat,aes(Cost,Gender))+labs(x="Cost",y="Gender")+ggtitle("Y vs X")+geom_point()+geom_smooth(method = "lm")
ggplot(data = newdat,aes(Cost,Complications))+labs(x="Cost",y="Complications")+ggtitle("Y vs X")+geom_point()+geom_smooth(method = "lm")

## Normality Check
par(mfrow=c(1,1))
qqnorm(modelOne$residual)
qqline(modelOne$residual)
shapiro.test(modelOne$residual)
## Equal Variance Check
par(mfrow=c(2,2))
plot(modelOne)
## Linearity Check
par(mfrow=c(2,3))
plot(x = Cost, y = modelOne$residual)
abline(h=0)
plot(x = Age, y = modelOne$residual)
abline(h=0)
plot(x = Gender, y = modelOne$residual)
abline(h=0)
plot(x = Interventions, y = modelOne$residual)
abline(h=0)
plot(x = Drugs, y = modelOne$residual)
abline(h=0)
plot(x = Duration, y = modelOne$residual)
abline(h=0)

```
#shapiro.test, boxcox, & transformation with Second Model
```{r, include=TRUE}
library(ggplot2)
library(MASS)
# not normal transform Cost
# we use lambda for transforming y (Cost) use box cox test 
# boxcox transformation 
boxcox(modelOne, data = newdat,lambda = seq(-1,1,0.1), optimize = TRUE, objective.name = "PPCC", eps = .Machine$double.eps, include.x = TRUE)
transform <- log(Cost)
modelTwo <- lm(log(Cost) ~ Age + Duration + Gender + Complications + Drugs + Interventions, data = newdat)
ress <- modelTwo$residuals
qqnorm(ress)
qqline(ress)
tran.fitted <- fitted(modelTwo)

summary(modelTwo)
anova(modelTwo) # provides ANOVA table for overall model

ggplot(data=newdat, aes(tran.fitted, ress))+labs(x="ei",y="transformed_yhat")+ggtitle("Ei vs Yhat")+geom_point()+geom_smooth(method = "lm")
ggplot(data=newdat, aes(transform, Age))+geom_point()+geom_smooth(method = "lm")
ggplot(data=newdat, aes(transform, Gender))+geom_point()+geom_smooth(method = "lm")
ggplot(data=newdat, aes(transform, Interventions))+geom_point()+geom_smooth(method = "lm")
ggplot(data=newdat, aes(transform, Drugs))+geom_point()+geom_smooth(method = "lm")
ggplot(data=newdat, aes(transform, Complications))+geom_point()+geom_smooth(method = "lm")
ggplot(data=newdat, aes(transform, Duration))+geom_point()+geom_smooth(method = "lm")


## Normality Check
par(mfrow=c(1,1))
qqnorm(modelTwo$residual)
qqline(modelTwo$residual)
shapiro.test(modelTwo$residual)
## Equal Variance Check
par(mfrow=c(2,2))
plot(modelTwo)
## Linearity Check
par(mfrow=c(2,3))
plot(x = Cost, y = modelTwo$residual)
abline(h=0)
plot(x = Age, y = modelTwo$residual)
abline(h=0)
plot(x = Gender, y = modelTwo$residual)
abline(h=0)
plot(x = Interventions, y = modelTwo$residual)
abline(h=0)
plot(x = Drugs, y = modelTwo$residual)
abline(h=0)
plot(x = Duration, y = modelTwo$residual)
abline(h=0)

shapiro.test(ress)

#Checking for vif > 10
# VIF Check
library(car)
vif(modelTwo)
```
# stepwise forward regression
```{r, include=TRUE}
library(olsrr)
library(car)
library(MASS)

#Here we are creating the best fit model, Forward Selection
Mod2ForReg <- ols_step_forward_p(modelTwo, details = TRUE, penter = 0.1)
plot(Mod2ForReg)
# final model
Mod2ForReg$model


#Doing it again but without a package so that we can verify the normality, varience, and linearity
## Forward Selection
null = lm(log(Cost) ~ 1, data = newdat) ## ~1 = intercept only
MLR <- step(null, scope = list(lower = null, upper = modelTwo), direction = "forward")
summary(MLR)
anova(MLR)

## Normality Check
par(mfrow=c(1,1))
qqnorm(MLR$residual)
qqline(MLR$residual)
shapiro.test(MLR$residual)

## Equal Variance Check
par(mfrow=c(2,2))
plot(MLR)

## Linearity Check
par(mfrow=c(2,2))
plot(x = Interventions, y = MLR$residual)
abline(h=0)
plot(x = Complications, y = MLR$residual)
abline(h=0)
plot(x = Duration, y = MLR$residual)
abline(h=0)

## VIF Check
vif(MLR)

## Step 5 Option 4: Stepwise Selection
step(modelTwo, data = newdat, direction = "both")
```