# Activity 1: Diamonds Dataset

require(ggplot2)
install.packages("glmnet", dependencies=TRUE)
require(glmnet)

data("diamonds")
summary(diamonds)
dim(diamonds)      #dimension of the dataset
