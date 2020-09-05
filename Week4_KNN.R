#################### K-Nearest Neighbours #########################

##### Iris dataset #####

iris = iris

# Prepare data by keeping only predictor variables Sepal Width & Petal Width
new_iris_predictors = iris[, c("Sepal.Width", "Petal.Width")]
# Subset all rows, response variable only
new_iris_class = iris[, "Species"]
# Find target observation x = (2.5, 1.5)
(target = which((iris$Sepal.Width==2.5)&(iris$Petal.Width==1.5)))
# Remove target observation
# Subsetting a single data row (row 73 of the predictors):
new_iris_test <- new_iris_predictors[target,]
# Subsetting all rows except row 73 (predictors):
new_iris_predictors <- new_iris_predictors[-target,]
# Subsetting all rows except row 73 (response variable): 
new_iris_class <- new_iris_class[-target]

# perform KNN classification of the test observation for very levels of k
library(class)
(knn(train=new_iris_predictors, test=new_iris_test, 
     cl=new_iris_class, k=1, prob=TRUE))
(knn(train=new_iris_predictors, test=new_iris_test, 
     cl=new_iris_class, k=3, prob=TRUE))
(knn(train=new_iris_predictors, test=new_iris_test, 
     cl=new_iris_class, k=5, prob=TRUE))
(knn(train=new_iris_predictors, test=new_iris_test, 
     cl=new_iris_class, k=7, prob=TRUE))

# Compute euclidean distance from x and sort in ascending order
dist(new_iris_predictors)
# Squared pairwise (Euclidean) distance matrix:
D <- as.matrix(dist(iris[, c("Sepal.Width", "Petal.Width")]))
# target = 73 from the previous codes
D_target <- D[target,] # Subsetting the 73rd row of the distance matrix 
sorted_indices <- order(D_target)
sorted_indices[1:10]
# check the distances from observation 73 to its 9 nearest neighbours by 
# subsetting D_target with the corresponding indices in sorted_indices:
D_target_sorted = D_target[sorted_indices]
D_target_sorted[1:10]
# The 6th, 7th and 8th nearest neighbours are tied at distance 0.2828427


##### Iris dataset (exercise 2) #####

#split data 80% training set and 20% test set
set.seed(0)
no_obs = dim(iris)[1]
# 20% test set
test_index = sample(no_obs, size = as.integer(no_obs*0.2), replace=FALSE)
test_predictors = iris[test_index, c("Sepal.Length", "Sepal.Width", 
                                     "Petal.Length", "Petal.Width")]
test_class = iris[test_index, "Species"]
# 80% training set
training_index = -test_index
training_predictors = iris[training_index, c("Sepal.Length", "Sepal.Width", 
                                         "Petal.Length", "Petal.Width")]
training_class=iris[training_index, "Species"]
# use the training set and a KNN classifier with k = 1 to classify the test set
Pred_class <- knn(train=training_predictors, test=test_predictors, 
                  cl=training_class, k=1) 
(cont_tab <- table(Pred_class, test_class))
(accuracy <- sum(diag(cont_tab))/sum(cont_tab))
# 2 out of 30 observations have been misclassified, accuracy of 93.3333333%

# Instead of  splitting the data into training and tests set only once,
# repeat this procedure 10 times (for loop)
set.seed(0)
# Create vector with 10 elements (initially 0) to store accuracy for each experiment 
accuracy <- rep(0,10)
for(i in 1:10){
  # 20% data records for test:
  test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) 
  test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width",
                                        "Petal.Length", "Petal.Width")] 
  test_class <- iris[test_index, "Species"]
  # 80% data records for training:
  training_index <- -test_index
  training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width",
                                                "Petal.Length", "Petal.Width")]
  training_class <- iris[training_index, "Species"]
  # Prediction and contingency table:
  Pred_class <- knn(train=training_predictors, test=test_predictors, cl=training_class, k=1)
  cont_tab <- table(Pred_class, test_class)
  # Store accuracy (ith experiment) in the ith vector cell:
  accuracy[i] <- sum(diag(cont_tab))/sum(cont_tab)
} 
accuracy
# compute the mean of all accuracy values stores in vector "accuracy":
mean(accuracy)
# compute the standard deviation of all accuracy values stores in vector "accuracy":
sd(accuracy)
