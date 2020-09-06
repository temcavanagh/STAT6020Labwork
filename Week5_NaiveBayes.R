install.packages("naivebayes")
library(naivebayes)

summary(Weather_Data_Witten_Frank_Book)
Weather_Data = Weather_Data_Witten_Frank_Book
NaiveBayesWeather = naive_bayes(Play~., data=Weather_Data)
NaiveBayesWeather

# Classify new observation Outlook=Sunny, Temperature=Cool, Humidity=High,  Windy=True

# Create test dataset (easiest way is to append at the end of the 
# original data set and subset afterwards):
New_Weather_Data <- rbind(Weather_Data,
                          c("Sunny","Cool","High","True","NA"), # Function rbind() appends rows
                          c("Rainy","Hot","High","False","NA")) # Unknown class labels left as missing (NA)
# Subsetting the appended data (rows 15 and 16) without the 
# class label column (5th column, Play, removed):
New_Weather_Data <- New_Weather_Data[15:16, -5] # Test data (predictors only) 
New_Weather_Data
# Perform classification of the new observations:
Pred_class <- predict(NaiveBayesWeather, newdata = New_Weather_Data, type = "class") # Confirm that the two observations have indeed been classified as "No":
Pred_class
# Find probabilities with type="prob"
Pred_prob <- predict(NaiveBayesWeather, newdata = New_Weather_Data, type = "prob") 
Pred_prob
# classify only the observations used for training use newdata = NULL
Pred_class <- predict(NaiveBayesWeather, newdata = NULL, type = "class") 
Pred_class
# compare the predicted classes against the real classes (5th column of the 
# original data set) by building a contingency table (confusion matrix)
cont_tab <- table(Pred_class, Weather_Data$Play) 
cont_tab
# Assess accuracy 
(reconstruction_accuracy <- sum(diag(cont_tab))/sum(cont_tab))
# Assess error
(reconstruction_error <- 1 - reconstruction_accuracy)


##### Naive Bayes with numerical predictors #####

data("iris")
summary(iris)
# split the data set into a training set 80% and a test set 20%
set.seed(0)
no_obs <- dim(iris)[1] # No. of observations (150)
# 20% data records for test:
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) 
# 80% data records for training:
training_index <- -test_index
# use the training set to train a Naive Bayes classifier
NaiveBayesIris <- naive_bayes(Species ~., data = iris[training_index,]) 
NaiveBayesIris
# assess the prediction power of classifier using data reserved for test
Pred_class <- predict(NaiveBayesIris, newdata = iris[test_index, -5], type = "class") 
(cont_tab <- table(Pred_class, iris$Species[test_index]))
((accuracy <- sum(diag(cont_tab))/sum(cont_tab)))
