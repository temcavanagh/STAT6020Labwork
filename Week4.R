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
training_dat$Approved <- as.factor(training_dat$Approved) # Converting char to factor

# fit lrm model, named LR_CRX
# Train the model
LR_CRX = glm(formula=Approved~., data=training_dat, family=binomial)
summary(LR_CRX)
# Make predictions (test data)
P_positive_test = predict.glm(LR_CRX, newdata=test_predictors, type="response")
# Apply threshold (transform probabilities into class label +1, -1)
Pred_class = sign(P_positive_test - 0.5)
# Build contingency table (predicted labels vs true labels)
cont_tab = table(Pred_class, test_class_labels)
cont_tab

# cont_tab shows the following:
# 69 true negatives
# 33 true positives
# 19 false negatives
# 9 false positives

# assess classification accuracy and classification error:
accuracy = (cont_tab[1,1] + cont_tab[2,2]) / 
  (cont_tab[1,1]+cont_tab[1,2]+cont_tab[2,1]+cont_tab[2,2])
accuracy
error = (cont_tab[1,2] + cont_tab[2,1]) / 
  (cont_tab[1,1]+cont_tab[1,2]+cont_tab[2,1]+cont_tab[2,2])
error

# or equivalently:
accuracy1 = sum(diag(cont_tab))/sum(cont_tab)
accuracy1
error1 = 1 - accuracy1
error1

summary(LR_CRX)

# Repeat above exercise using only variables YearsEmployed, CreditScore, 
# and AccountBalance as predictors.

LR_CRX = glm(formula=Approved~YearsEmployed+CreditScore+AccountBalance, 
             data=training_dat, family=binomial)
P_positive_test = predict.glm(LR_CRX, newdata=test_predictors, type="response")
Pred_class = sign(P_positive_test - 0.5)
cont_tab = table(Pred_class, test_class_labels)
accuracy = sum(diag(cont_tab))/sum(cont_tab)
accuracy
error = 1 - accuracy
error
summary(LR_CRX)

# compute probability that a person employed for 5 years, with credit score of 6, 
# and account balance of 10000, will get credit application approved (class positive, +)
# Eq. (2), the required probability is given by P = 1/(1 + e−(β0+β1×5+β2×6+β3×10000))

beta0 = summary(LR_CRX)$coefficients[1,1] #Intercept
beta1 = summary(LR_CRX)$coefficients[2,1] #YearsEmployed
beta2 = summary(LR_CRX)$coefficients[3,1] #CreditScore
beta3 = summary(LR_CRX)$coefficients[4,1] #AccountBalance

Prob1 = 1/(1 + exp(-(beta0 + beta1*5 + beta2*6 + beta3*10000)))

# Repeat calculation using predict.glm()
test_data = data.frame(YearsEmployed=5, CreditScore=6, AccountBalance=10000)
predict.glm(LR_CRX, newdata = test_data, type = "response")

# Calculate for person 1/2 year employed, 0 account balance, credit score of 3
Prob2 = 1/(1 + exp(-(beta0 + beta1*0.5 + beta2*3 + beta3*0)))

# Repeat using predict.glm()
test_data = data.frame(YearsEmployed=0.5, CreditScore=3, AccountBalance=0)
predict.glm(LR_CRX, newdata = test_data, type = "response")

# how much will the log-odds — Eq. (7) — of credit approval increase or 
# decrease if the person stays 1 additional year employed
beta1
