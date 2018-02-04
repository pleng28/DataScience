###### Short Lab: Exploring the "boosting" function from the "adabag" library ##########
## Below is the code we explored last week for detecting breast cancer using boosting.
## (1) Review the commands and compile a list of questions you have about the code (if any).

############################### Last Week's Boosting Code ##############################
#################### Experimenting with AdaBoost ############################
## Install new packages and library all packages needed for this script.
install.packages("adabag")
install.packages("plyr")
install.packages("ipred")
install.packages("pROC")
library(plyr)
library(dplyr)
library(ggplot2)
library(ipred)
library(adabag)
library(pROC)
library(randomForest)

## Read in the data
bc <- read.table("~/BreastCancerData.csv",sep = ",", header = TRUE)

## Review a summary of the data to get a sense of the missingness/cleanliness of the data and to learn
## what the predictors and outcome variables are. Refer to data dictionary and other sources to make sense
## of these variables.
summary(bc)

## Use the dataset description supplied in your folder, Google, and 
## https://gsm672.wikispaces.com/Prediction+of+Breast+cancer to 
## help you understand the meaning behind the data.

## How are the samples gathered?  Mammography?
## What's the importance of mitosis in determining malignancy?

## Uh, oh.  Looks like something strange may be going on with Bare.Nuclei.  Let's investigate.
bc$Bare.Nuclei

## What's wrong?  
## Let's fix it.
## The below command says for the feature Bare.Nuclei replace "?"s with NA
bc$Bare.Nuclei[bc$Bare.Nuclei=="?"] =NA
## Make sure Bare.Nuclei is numeric:
bc$Bare.Nuclei <- as.numeric(bc$Bare.Nuclei)

## Now do things look better?
summary(bc)

## What to do with NAs?
sum(is.na(bc))

## Because there are not many NAs, one option is to remove all samples that contain any NAs.
## I only want complete cases of bc (i.e, complete observations, all columns present)
bc <- bc[complete.cases(bc), ]
sum(is.na(bc))

## We require one final fix to our data.  The class is being read as a numerical value, and since we are
## interested in classification, we need to fix this.

bc$Class <- as.factor(bc$Class)

## The current level labels are not very clear to us.  Let's make them meaningful.
levels(bc$Class)
levels(bc$Class) <- c("benign","malignant")
summary(bc)
## Now I will separate my data into testing and training.  I need to know the number of rows to do this.
nrow(bc)
set.seed(12345)
bc_rand <- bc[order(runif(683)), ]

# split the data frames
bc_train <- bc_rand[1:550, ]
bc_test  <- bc_rand[551:683, ]

## Now let's give boosting a try, using the "adabag" package in R.

adaboost<-boosting(Class ~ ., data=bc_train, boos=FALSE, mfinal=20,coeflearn='Freund')

## Let's explore the adaboost object:

summary(adaboost)

adaboost$trees
t1<-adaboost$trees[[1]]
install.packages("tree")
library(tree)
plot(t1)
text(t1,pretty=0)
adaboost$weights
adaboost$importance

## We can also study the error evolution as boosting moved through each of the 20 weak learners.
errorevol(adaboost,bc_train)


## Now we can make a prediction!

p <- predict(adaboost,bc_test[,-10])

## Notice p has multiple features (because "predict" in the adabag package is different than base "predict")
## Look at these features.

## We can study our performance using our classic confusion matrix summary:
library(gmodels)
CrossTable(bc_test$Class, p$class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diagnosis', 'predicted diagnosis'))

## We can also use ROC curves.  How is the ROC curve created?

RC <- roc(bc_test$Class,p$prob[,2])
plot(RC, legacy.axes = TRUE)

################# End Last Week's Boosting Code ######################################
## Please answer the following:

## 2) How do you think the probabilities columns in the boosting object are calculated?

adaboost$prob

## The probabilities columns in the boostin object are calculated by the adaboost function using weighted votes divieded by the sum of the weights



## 3) Why does the probability feature have two columns?  Why do we pick the second column
##    for ROC analysis?

## The probability feature has two columns because the first one is benign and the second column is malignant. We pick the second column for ROC analysis because malignancy is rarer and we are trying to find malignantcy versus benign. 
## they add up to one in every single row
## malignant is the rarer event and what we are interested in and it is the second column




## 4) Suppose you took an observation from your test set.  How could you recapitulate its classification
##    in the classification feature using the tree feature of the boosting object and the weight feature?


## After you run the malignant and benign (+1, and -1), and then run through the specific observation through the first tree and multiply it by the weight, then repeat with the second tree up until the 20th tree. 
## Run it through each tree and multiply the output by the weights
## If the weighted sum is postive then it is malignant if it is negative then it is benign
## the weights are the stage 

## 5) How do the features of the boosting object relate to our discussion at the board last week and
##    things like the scaling factor and the updated weights of the training observations?


## A bunch of features one is weight which represents stage for a tree 
## the probabilities give you the probability of benign in the first column and probability of malignancy in the second column
## the final classification relates to the ensemble we came up with
## We look at how often a split is made in a given feature and we multiply it by the square of the improvement it gives to the algorithms for importance
## another way to get importance different to random forrest
## a call feature to show us what we called




## 6) What is special about the predict function in the adabag package (i.e., how is it different from the
##    predict function we've used for random forests, etc.)?

##It has multiple features one of which is probability and it also has a classification and we can also coerce from other predicts
## the predict function in the adabag package is simular to the probabilities in that it is the number of trees 
