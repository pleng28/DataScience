## ----  Using Decision Trees  -------------------

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("~/Desktop/DS 4559 2017/credit_PACKT.csv")
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

# compare the credit and credit_rand data frames
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)

# split the data frames
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree
install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[,-17], credit_train$default)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test[,-17])

# cross tabulation of predicted versus actual classes
install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

install.packages("ipred")
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit_train, nbagg = 25)## Training the bagging model.  Notation implies
## default depends on all predictor variables ( ". ~").  I am using 25 trees in this particular model
credit_pred <- predict(mybag, credit_test[,-17])## Using the trained model to make prediction on my 
## test data.  I omit the last column ("[,-17]") as this is the output variable.
table(credit_pred, credit_test$default) ## confusion matrix for performance.  Always comparing predicted
## oucome for test data versus the actual outcome (credit_test$default or credit_test[,17])

install.packages("randomForest")
library(randomForest)

## First, I create the random forest model, using training data without the last column(
##(my outcome column) as the first argument.  My second argument is the output of the
## training data (credit_train$default or credit_train[,17]).  ntree specifies the number of trees
## to be created in the forest.  mtry is the number of predictor variables each tree will consider.

m <- randomForest(credit_train[,-17], credit_train$default, ntree = 500, mtry = sqrt(16))

## Prediction step.  First argument is the model we created with the training data (above), m.
## The second argument is the test data without the outcome variable.
p <- predict(m, credit_test[,-17])

## Display results:
library(gmodels)
CrossTable(credit_test$default, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))









install.packages("caret")
library(caret)
varImp(m)
varImpPlot(m,type=2)


