library(ggplot2)

cherry=read.csv("cherry.csv")
summary(cherry)

# Regression
cherry.lm=lm(Volume~Diam+Height,data=cherry)
summary(cherry.lm)

# Plot residuals
plot(cherry.lm, which=1)
# Indicates non-linear relationship, smiley face shape

# Model with squared diameter
cherry.lm2 = lm(Volume~Diam+I(Diam^2)+Height,data=cherry)
summary(cherry.lm2)
plot(cherry.lm2, which=1)
# Residual plot shows that non-linear relationship has been properly captured


# Build matrix X
X=matrix(c(1,0,1,2), 2, 2, byrow=TRUE)
X

# Build vector Y
Y=matrix(c(1.5,5.5), 2, 1, byrow=TRUE)
Y

# Compute least squares
# t() = transpose; solve() = matrix inverse; %*% = Matrix multiplication
BETA = solve(t(X)%*%X)%*%t(X)%*%Y
BETA


# Exercise 1: Multiple linear regression

set.seed(0)

# 100 observations of predictor x1 randomly drawn from [0,1]
x1 = runif(n=100, min=0, max=1)

# 100 observations of predictor x2 randomly drawn from [0,1]
x2 = runif(n=100, min=0, max=1)

#Build vector Y=5+3x1+7x2+epsilon
Yi=5+3*x1+7*x2+rnorm(n=100,mean=0,sd=1)

# Build data matrix
dat = matrix(c(x1,x2,Yi), nrow=100, ncol=3, byrow=FALSE)
dat

# Task: Compute least squares coefficients using dat matrix

Y = dat[,3] # save 3rd column of matrix as Y
X = dat[,1:2] # save 1st and 2nd col as X (2 predictors)
X = cbind(rep(1,100), X) # insert column of 1's
BETA = solve(t(X)%*%X)%*%t(X)%*%Y # compute coefficients
BETA


# Exercise 2

cherry=read.csv("cherry.csv")
cherry=as.matrix(cherry)
cherry

# Volume as a function of Diam and Height
Y=cherry[,3]
X=cherry[,1:2]
X=cbind(rep(1,31), X)
BETA = solve(t(X)%*%X)%*%t(X)%*%Y # compute coefficients
BETA

# Volume as a function of Diam, Diam squared and Height
Diam2 = cherry[,1]^2
X = X=cbind(X, Diam2)
BETA = solve(t(X)%*%X)%*%t(X)%*%Y
BETA
