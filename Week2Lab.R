# Part A: Simple Linear Regression

# Activity 1: Blood pressue

library(ggplot2)
bp=read.csv("bloodpressure.csv")
summary(bp)

# Plot the data
ggplot(bp,aes(x=Age,y=SystolicBloodPressure))+
  geom_point(shape=19)+geom_smooth(method="lm",se=FALSE)

# Calculate the regression model
bp_model=lm(formula = SystolicBloodPressure~Age, data=bp)
summary(bp_model)

# Plot the residuals
plot(bp_model, which=1)


# Activity 2: Using up soap

soap=read.csv("soap.csv")
summary(soap)

# Plot the data
ggplot(soap,aes(x=Day,y=Weight)) + 
  geom_point(shape=19) + geom_smooth(method="lm", se=FALSE)

# Calculate the regression line
soap_model=lm(formula = Weight~Day, data=soap)
summary(soap_model)

# Plot the residuals
plot(soap_model, which=1)

ggplot(soap,aes(x=Day,y=Weight)) + geom_point(shape=19)


# Part B: Multiple Linear Regression

# Activity B1

softdrink = read.csv("softdrink.csv")
summary(softdrink)

# Plot the data
ggplot(softdrink,aes(x=Cases,y=Time))+ 
  geom_point(shape=19)+ geom_smooth(method="lm")

ggplot(softdrink,aes(y=Time,x=Distance))+ 
  geom_point(shape=19)+ geom_smooth(method="lm")

# Perform multiple linear regression analysis
softdrink.fit = lm(Time~Cases+Distance,data=softdrink)
summary(softdrink.fit)

# Plot the residuals
plot(softdrink.fit, which=1)
