#Clean Environment
remove(list = ls())

#Install and call the required packages
install.packages("party")
install.packages("caret")
install.packages("tidyverse")
install.packages("skimr")
install.packages("tidymodels")
library(tidymodels)
library(party)
library(tidyverse)
library(caret)
library(skimr)

#Reading the file into a dataframe
iris <- read.csv("iris.csv", header = T)

#Exploratory analysis on the data frame
head(iris)
tail(iris)
glimpse(iris)
dim(iris)
skim(iris)
str(iris)

#Checking for null values
colSums(is.na(iris))

#Converting the dependent variable into a factor
iris$Species <- as.factor(iris$Species)
levels(iris$Species)
class(iris$Species)


set.seed(100)

#Partitioning the data frame for training and testing the model
pd_iris <-sample(2, nrow(iris), replace = T, prob = c(0.8, 0.2))
iris_train <- iris[pd_iris == 1,]
iris_validate <- iris[pd_iris ==2,]

#Creating the decision tree model
iris_tree <- ctree(Species ~ SepalLengthCm + SepalWidthCm + PetalLengthCm + PetalWidthCm, data = iris_train)

#Visualizing the model
plot(iris_tree)

#Creating a table of the predicted and actual values on the training data
tab_train <- table(predict(iris_tree), iris_train$Species) 

#Calculating the accuracy of the model
acc_train <- sum(diag(tab_train)) / sum(tab_train)
acc_train

#Creating a table of the predicted and actual values on the test data
tab_test <- table(predict(iris_tree, newdata = iris_validate), iris_validate$Species)

#Calculating the accuracy on the test data.
acc_test <- sum(diag(tab_test)) / sum(tab_test)
acc_test


