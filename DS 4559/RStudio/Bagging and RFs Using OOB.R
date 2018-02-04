## ----  Using Decision Trees  -------------------

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("~/Desktop/DS 4559 2017/credit_PACKT.csv")
str(credit)


## When calculating out-of-bag performance, there is no need to break data into testing and 
## training.  Why?

## Training the bagging model.  Notation implies
## default depends on all predictor variables ( "~ .").  I am using 25 trees in this 
##particular model.  I am using the full data set and setting the argument "coob" to TRUE.


library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, coob = TRUE, nbagg = 200)
mybag

## Next, we train the random forest on the full dataset:

library(randomForest)



m <- randomForest(credit[,-17], credit$default, ntree = 500, mtry = sqrt(16), importance = TRUE)
#
# The next line displays the out-of-bag accuracy:

print(m) 










install.packages("caret")
library(caret)
varImp(m)
varImpPlot(m,type=2)


