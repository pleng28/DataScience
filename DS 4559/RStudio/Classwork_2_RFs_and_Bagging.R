## Bagging and Random Forest Classwork Example###
## Let's begin by creating several bootstrapped samples of the iris dataset to study
## the decision tree models and predictions they make:
## First, randomize and create a small test set, simply to serve our observation
## purposes:
install.packages("png")
library(C50)
library(png)
iris_rand <- iris[order(runif(150)), ]

iris_train <- iris_rand[1:140,]
iris_test <- iris_rand[141:150,]

## Now we sample the training data with replacement and create a bootstrapped dataset
## Dataset 1:

set.seed(123)
index <- sample(1:nrow(iris_train), 140, replace = TRUE)
## View the indices:
index
## Check to see how many of the original data were used in the sample and the proportion
## of the total original data this represents:
length(unique(index))
length(unique(index))/140

## Create a new set of iris data using the bootsampled indices:
rr1 <- iris_train[index, ]
rr1

## Create a tree model from this bootstrapped set:
m1 <- C5.0(x=rr1[,-5], y=rr1$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
## View the model:
plot(m1)

## Now we create a new bootstrapped sample, as above, and train a tree model on the
## data:
set.seed(145)
index1 <- sample(1:nrow(iris_train), 140, replace = TRUE)
index1
length(unique(index1))
length(unique(index1))/140

rr2 <- iris[index1, ]
rr2
m2 <- C5.0(x=rr2[,-5], y=rr2$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
plot(m2)

## Now we make predictions on the bootstrapped data samples:
p1 <-predict(m1,newdata=iris_test[,-5])
p1

p2 <-predict(m2,newdata=iris_test[,-5])
p2

## Using the same method, create 5 more bootstrapped samples and their corresponding
## trees and predictions.  Visually inspect your predictions and see the variation
## among them.  Finally, visually/manually tabulate a vote for each of the 10
## outcomes. Compare this to the actual Species for the 10 test examples.
## 
## The trees were too small which causes the predictions to be simular because they were pruned too early. The decision trees are different though because they have different seeds 

set.seed(100)
index2 <- sample(1:nrow(iris_train), 140, replace = TRUE)
index2
length(unique(index2))
length(unique(index2))/140

rr3 <- iris[index1, ]
rr3
m3 <- C5.0(x=rr3[,-5], y=rr2$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
plot(m3)

set.seed(101)
index3 <- sample(1:nrow(iris_train), 140, replace = TRUE)
index3
length(unique(index3))
length(unique(index3))/140

rr4 <- iris[index1, ]
rr4
m6 <- C5.0(x=rr3[,-5], y=rr4$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
plot(m6)

set.seed(103)
index4 <- sample(1:nrow(iris_train), 140, replace = TRUE)
index4
length(unique(index4))
length(unique(index4))/140

rr4 <- iris[index1, ]
rr4
m4 <- C5.0(x=rr3[,-5], y=rr4$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
plot(m4)

set.seed(104)
index5 <- sample(1:nrow(iris_train), 140, replace = TRUE)
index5
length(unique(index5))
length(unique(index5))/140

rr5 <- iris[index5, ]
rr5
m5 <- C5.0(x=rr3[,-5], y=rr5$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
plot(m5)



## We will use a wine dataset to predict the quality of different wines and compare our results against
## the quality as determined by several sommeliers.
## We begin by importing the data:
url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
wine <- read.csv("C:/Users/LHZ/Desktop/winequality-white.csv")

## Let's examine the contents of the dataset:

head(wine)

## Make a barplot of wine quality:
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

ggplot(data=wine,mapping=aes(x=quality))+geom_bar()

## Check that there are no missing values or strange, outlying variables:

sum(is.na(wine))



## Change the output variable of the data into factors of the type good, bad, and normal as follows:
## 1 through 5 is 'bad', greater than 6 is 'good', and 6 is 'normal'.  Then look at the total number
## of wines that fall into each group.

##mutate 
library(dplyr)

ifelse(quality<6, quality = bad)
normal <-mutate(wine, if(quality=6)quality = normal)
good <- mutate(wine,if(quality>6)quality=good)






## How might you check that the changes you made took place as expected?:


##

## I would look at the data and compare the totals of 1-5 with the totals for bad and the totals for 7 and above as good and the 6 total with normal

## Randomize your data and break them into training and test sets:

randset <- 
set.seed(4)
out <- permatswap(mat, times = 99, burnin = 20000, thin = 500, mtype = "prab")




##




## Using appropriate libraries, create an ID3 tree model from your training data and make predictions
## on the test data.  Show your results in a confusion matrix and report your accuracy as an
## annotation below:

set.seed(145)
index1 <- sample(1:nrow(iris_train), 140, replace = TRUE)
index1
length(unique(index1))
length(unique(index1))/140

rr2 <- iris[index1, ]
rr2
m2 <- C5.0(x=rr2[,-5], y=rr2$Species,earlyStopping = FALSE,noGlobalPruning= TRUE)
plot(m2)

library(randomForest)
library(C50)
m<- C5.0(wine_train[-12], wine_train$quality)
p <- predict(m,wine_test[-12])

m<- randomForest(wine_train[, !names(wine_test) %in% c("quality", "wine_quality"))] wine_train$wine_quality, ntree = 500, mtre
library(gmodels)

CrossTable(wine_test$winequality, p,
            prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE,
            dnn = c('actual default', 'predicted default'))

##accuracy from he confusion matrix is about 72 percent

##



## Using appropriate libraries, first create and make predictions on a bagged model and display results,
## reporting accuracy.  Then use the full dataset to do the same for a random forst model, by displaying OOB accuracy.
##Feel free to play with the hyperparameters of each model to maximize performance.


##
mybag <- bagging(wine_quality~., data=wine_train[,-12],nbagg=25)
library(randomForest)

ggplot(data = wine) + geom_histogram(mapping = aes(x=fixed.acidity))
ggplot(data = wine) + geom_histogram(mapping = aes(x=alcohol))
ggplot(data = wine) + geom_histogram(mapping = aes(x=residual.sugar))






## What are the three most important variables in determining the quality of a wine?


## fixed acidity, citric acid, residual sugar

library(C50)
m <- C5.0(wine_train[-12])




















