########### Guided Lab:  Cross-Validation #########################################
## First, let's experiment with several new functions that will help you understand
## the cross-validation code below.

## First concept: the "rep" function
## This function allows you to repeat a value multiple times.  Let's look at an example:

rep(1:4,2)

## Notice the output gives you 1:4 twice.  The first argument is/are the value(s) you wish
## to repeat.  The second argument indicates the number of times you wish to replicate the
## value.
## What do you expect rep(0,130) to create?  Try it:

rep(0,130)
#############################################################################################
## Second concept/new function: the "which" function

## We'll start by making up a toy vector for the purposes of demonstration:

louise <- c(1,2,4,7,8,10,2)

## Suppose we only want to retain the values for which louise is greater than 2.  We can use
## a which function for this:

louise <- louise[which(louise > 2)]
louise

## What about a which function that allows all values except 2?

## Reset "louise"

louise <- c(1,2,4,7,8,10,2)
## Now use the which function to pull out all values of louise exept
## for 2:

louise <- louise[which(louise != 2)]
louise

## We can do the same with a data frame feature:

data("iris")

## Suppose I only wish to look at a subset of the iris data for which the Sepal.Length is > 7.
## Notice the comma after the which command
iris <- iris$Sepal.Length[which(iris$Sepal.Length > 7)]

## Note that you could also use a filter function to perform the same analysis!

library(tidyverse)
data("iris")
iris %>% filter(Sepal.Length>7)

####################################################################################################
## Third concept: for loops

## For loops are useful.  Let's take a look at how they operate in R:

for (i in 1:10){
  k = i+10
  print(k)
}

####################################################################################################
## Fifth concept: the "cvfolds" object
install.packages("cvTools")
library(cvTools)
k <- 10
folds <- cvFolds(25,k)
summary(folds)

folds$n
folds$K
folds$R
folds$subsets
folds$which

## Now try folds$subsets[folds$which i = 1]

folds$subsets[folds$which != 1]

## What does the output mean??



## Okay, now it's time to run some code that implements cvFolds!!

install.packages("cvTools")
library(cvTools) #run the above line if you don't have this library

k <- 10 #the number of folds
dataset <- iris
set.seed(1234)
dataset_rand <- dataset[order(runif(150)),]

## Divide data into k folds:

folds <- cvFolds(NROW(dataset), K=k)
summary(dataset_rand)

## Now we append a new column, holdoutpred, to dataset_rand using the dollar sign ($) notation.  Notice I use the "rep" 
## command and set all the new column values equal to 0.

dataset_rand$holdoutpred <- rep(0,nrow(dataset_rand))

## We are going to create a simple decision tree using cross-validation just to keep things as simple as possible.

library(C50)

## Here's my for loop that I run from 1 to the specified number of folds (usually 10):

for(i in 1:k){
  ## Note that the syntax below is the syntax we investigated above.
  
  train_data <- dataset_rand[folds$subsets[folds$which != i], ] #Set the training set.  Note that it contains all folds
  ## except for the fold reserved for test data
  test_data <- dataset_rand[folds$subsets[folds$which == i], ] #Set the test set.  This set contains only
  ## one fold, namely the ith fold.
  
  ## Create a simple decision tree model as usual:
  newmod <- C5.0(Species ~ .,data=train_data) #Get your new linear model (just fit on the train data)
  newpred <- predict(newmod,newdata=test_data[,-5]) #Get the predicitons for the test set 
  ## (from the model just fit on the train data)
  
  dataset_rand[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for 
  ## later use.
}
dataset_rand$holdoutpred
table(dataset_rand$holdoutpred,dataset_rand$Species)

