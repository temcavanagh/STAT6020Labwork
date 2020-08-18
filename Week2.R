# Load required libraries
require(ggplot2)

# Load data - working directory set
bac = read.csv('bac.csv')

# Plot simple linear model
ggplot(bac,aes(x=Beers,y=BAC)) +
  geom_point(shape=19) +
  geom_smooth(method = 'lm', se=FALSE)

# Linear model of BAC as a function of Beers
bac.lm <- lm(BAC~Beers,data=bac)
# Show summary of output regression
summary(bac.lm)
# Estimated coefficient beta_0
summary(bac.lm)$coefficients[1,1]
# Estimated coefficient beta_1
summary(bac.lm)$coefficients[2,1]
# Estimated p-value beta_1
summary(bac.lm)$coefficients[2,4]
# Produce a residual plot of the regression
# which = 1 selects the residual plot from the available plots for an "lm" object
plot(bac.lm, which=1)

# Exercise 1: BAC

# Compute linear regression using equations
# Step 1 Calculate means, stdev, correlation coefficients
meanBeers = mean(bac$Beers)
  (meanBeers)
meanBAC = mean(bac$BAC)
  (meanBAC)
sdBeers = sd(bac$Beers)
  (sdBeers)
sdBAC = sd(bac$BAC)
  (sdBAC)
r = cor(bac$Beers,bac$BAC)
  (r)
B1 = ((r)*(sdBAC/sdBeers))
  (B1)
B0 = (meanBAC-(B1*meanBeers))
  (B0)
# Compute fitted values Y hat
Yi_hat = (B0 + B1*bac$Beers)
  (Yi_hat)
# Compute residuals
ei = (bac$BAC - Yi_hat)
  (ei)
# Compute R squared
R2 = cor(bac$Beers,bac$BAC)^2
  (R2)
# Plot residuals against x values
plot(bac$Beers,ei)
# Plot residuals against estimated y values
plot(Yi_hat,ei)


# Exercise 2: Diamonds

# Perform linear regression of price against weight
require(ggplot2)

diam.lm = lm(formula = price~carat,data=diamonds)
summary(diam.lm)

plot(diam.lm,which=1)

plot(diamonds$carat,diamonds$price)

# Apply log transformation to input and output
dia = diamonds
dia$price = log(dia$price)
dia$carat = log(dia$carat)
dia.lm = lm(formula = price~carat,data=dia)
summary(dia.lm)

# Repeat using the formulas
sumx2 = sum(diamonds$carat^2)
sumY = sum(diamonds$price)
sumx = sum(diamonds$carat)
sumxY = sum(diamonds$carat*diamonds$price)
n = dim(diamonds)[1]
B0 = (sumx2*sumY - sumx*sumxY)/(n*sumx2 - sumx^2)
B1 = (n*sumxY - sumx*sumY)/(n*sumx2 - sumx^2)
Y_hat = B0 + B1*diamonds$carat
ei = diamonds$price - Y_hat
R2 = cor(diamonds$carat,diamonds$price)^2
plot(diamonds$carat,ei)
plot(Y_hat,ei)
