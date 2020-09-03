# Week 4 Logistic Regression & KNN

# Lecture examples

# build an artificial data set with 20 observations, 
# 10 from each class (positive, +1, or negative, -1), 
# described by a single predictor, x.

set.seed(0)
x0_10 = runif(10, min=0, max=60); x11_20 = runif(10, min=40, max=100)
y0_10 = rep(-1,10); y11_20 = rep(+1,10)
( dat = data.frame(x=c(x0_10,x11_20), y=c(y0_10,y11_20)))


# Fit a linear model and perform classification
# as.factors() converts the numerical variable (with values +1, -1) into a 
# categorical variable (with factor levels "1", "-1")
# Variable "class" stores the resulting categorical class labels

library(ggplot2)
class = as.factor(dat$y) 
(g = ggplot() + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class) ))


# Logistic regression model

# Append to training dataset the class labels as an additional column/variable:
dat <- cbind(dat, class)
# Train the LR model (class=response, x=predictor):
LR_model <- glm(formula=class~x, data=dat, family=binomial)
# Compute class probabilities (for training data):
P_positive_dat <- predict.glm(LR_model, type="response")
# Apply threshold (transform prob. into predicted class labels +1, -1):
Y_dat <- sign(P_positive_dat - 0.5)
# Build test dataset:
new_dat <- data.frame(x=seq(from=0, to=100, by=0.1))
# Class probabilities (test data):
P_positive_new_dat <- predict.glm(LR_model, newdata=new_dat, type="response")


# Practical examples

# CBA 1984 credit card applications 

# Read data
Data <- read.table( 
  "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
  header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = "?")
# Add variable names
names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", 
                 "Occupation", "BankingInstitution", "YearsEmployed", "NoPriorDefault", 
                 "Employed", "CreditScore", "DriversLicense", "AccountType", 
                 "MonthlyIncome", "AccountBalance", "Approved")
# remove rows with na values
Data = na.omit(Data)
# Only use numerical variables
numeric_predictors <- c("Age","MonthlyExpenses","YearsEmployed","CreditScore",
                        "MonthlyIncome","AccountBalance")
# split 80% training set, and 20% test set
set.seed(0)
no_obs <- dim(Data)[1] # Number of rows/observations in the dataset
# 20% data records for test:
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) 
# Subsetting the data (test rows, numeric predictors):
test_predictors <- Data[test_index, numeric_predictors]
# Subsetting the data (test rows, response variable):
test_class_labels <- Data[test_index, "Approved"]
# 80% remaining data records (for training):
training_index <- -test_index
# Subsetting the data: training rows, numerical predictors and response variable:
training_dat <- Data[training_index, c(numeric_predictors, "Approved")]
