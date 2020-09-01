# Activity 1: Diamonds Dataset

require(ggplot2)
install.packages("glmnet", dependencies=TRUE)
require(glmnet)

data("diamonds")
summary(diamonds)
dim(diamonds)      #dimension of the dataset

diamonds.lm = lm(formula = log(price) ~ carat+depth+table+x+y+z, data=diamonds)
summary(diamonds.lm)

# small p-value of the F-statistic suggests relationship between 
# one or more predictors and the response. 
# p-values for individual predictors are all statistically significant

# Create 3 random variables
set.seed(0)
P1 = rnorm(n=53940, mean=0, sd=1)
P2 = rnorm(n=53940, mean=0, sd=1)
P3 = rnorm(n=53940, mean=0, sd=1)

# Append random variables to datasets
diamonds_ext = cbind(diamonds,P1,P2,P3)

# Repeat least squares modelling with new dataset
diamonds_ext.lm = lm(formula = log(price) ~ carat+depth+table+x+y+z+P1+P2+P3,
                     data=diamonds_ext)
summary(diamonds_ext.lm)

# p-values for 3 new variables are not statistically significant

# Apply Ridge Regression and the Lasso 
X = model.matrix(log(price) ~ carat+depth+table+x+y+z+P1+P2+P3,
                 data=diamonds_ext)
head(X)   # View first few rows of X
X = X[,-1] # Remove first column of X
head(X)

# Get response variable ready (price) as corresponding column
Y = diamonds_ext$price
Y = log(Y)

# Perform Ridge Regression by calling glmnet w/ alpha=0
ridge = glmnet(X,Y,alpha = 0)
plot(ridge,label=TRUE)

# Run cross validation for ridge & visualise
cv_ridge = cv.glmnet(X,Y,alpha=0)
plot(cv_ridge)

# Check lambda values
cv_ridge$lambda.min
cv_ridge$lambda.1se
cv_ridge$lambda

# Check coefficients for each model
coef(cv_ridge, s=cv_ridge$lambda.min)
coef(cv_ridge, s = cv_ridge$lambda.1se)
coef(cv_ridge)

# coef(cv_ridge) == coef(cv_ridge, s = cv_ridge$lambda.1se)
# coef for 3 random variables are small but not zero
# Because ridge regression performs regularisation but not variable selection

# Perform using Lasso (alpha=1) instead 
lasso=glmnet(X,Y,alpha = 1)
plot(lasso,label = TRUE)

# Run cross validation
cv_lasso = cv.glmnet(X,Y,alpha=1)
plot(cv_lasso)

# Check lambda values
cv_lasso$lambda.min
cv_lasso$lambda.1se
cv_lasso$lambda

# Check coefficients
coef(cv_lasso)
# See that coefficients (including 3 random variables) are shrunk to zero

# Compare best cross-validated MSE for ridge and lasso
min(cv_ridge$cvm)
min(cv_lasso$cvm)
# See that lasso achieves smaller estimated error in the dataset




# Activity 2: American Football Punting dataset

summary(punting)
dim(punting)

# (i) Distance as a response for all variables except Hang
# (a) Standard Least Squares
punting.lm = lm(formula = Distance ~ . -Hang, data = punting)
summary(punting.lm)
# Individual results are not statistically significant

# (b) Ridge regression
X = model.matrix(Distance ~ . -Hang, data=punting)
X = X[,-1]
Y = punting$Distance
ridge=glmnet(X,Y,alpha=0)
plot(ridge,label=TRUE)
cv_ridge = cv.glmnet(X,Y,alpha=0)
plot(cv_ridge)
cv_ridge$lambda.min
cv_ridge$lambda.1se
coef(cv_ridge)
# cross-validated plot shows that regularisation improves the test MSE, 
# which reaches a minimum before it starts increasing again
# However, all model coefficients are non-zero

# (c) lasso
lasso = glmnet(X,Y,alpha=1)
plot(lasso,label = TRUE)
cv_lasso = cv.glmnet(X,Y,alpha=1)
plot(cv_lasso)
cv_lasso$lambda.min
cv_lasso$lambda.1se
coef(cv_lasso)
# Ridge regression appears to be the best


# (ii) Hang as a function of all variables except Distance.

summary(punting)
dim(punting)

# (a) Standard Least Squares
punting.lm = lm(formula = Hang ~. -Distance, data = punting)
summary(punting.lm)
# Adjusted R2 and p-values suggest a slightly stronger relationship between 
# the predictors and Hang as response, when compared to Distance as response

# (b) Ridge regression
X <- model.matrix(Hang ~. -Distance, data = punting)
X <- X[,-1]
Y <- punting$Hang
# Ridge Regression
ridge = glmnet(X,Y,alpha=0)
plot(ridge,label=TRUE)
cv_ridge = cv.glmnet(X,Y,alpha=0)
plot(cv_ridge)
cv_ridge$lambda.min
cv_ridge$lambda.1se
coef(cv_ridge)

# (c) lasso
lasso = glmnet(X,Y,alpha=1) 
plot(lasso,label=TRUE)
cv_lasso = cv.glmnet(X,Y,alpha=1) 
plot(cv_lasso)
cv_lasso$lambda.min
cv_lasso$lambda.1se
coef(cv_lasso)
# minimal  difference between performance of Ridge Regression and  Lasso
# Lasso results in a simpler model though, with only 3 predictors
