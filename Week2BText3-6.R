# ISLR Chapter 3.6

library(MASS)
library(ISLR)

# Simple linear regression
fix(Boston)
names(Boston)
?Boston # Find out more about library

lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit) # Confidence intervals estimate 

# Produce confidence and prediction intervals of Y against x
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval ="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval ="prediction")

# Plot using leaset squares regression
plot(lstat,medv)
abline(lm.fit)
# Evidence of non-linearity in relationship 

# Create different plot lines and use different plotting symbols
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red") > plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# Diagnostic plots
par(mfrow=c(2,2)) # Partition plot view into 2x2 grid layout
plot(lm.fit)

# Alternatively compute linear regression residuals   
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# Evidence of non-linearity

# Leverage statistics used for predictors using hatvalues() function
plot(hatvalues (lm.fit))

# Identify index of largest element of vector using which.max()
which.max(hatvalues (lm.fit))


# Multiple linear regression
