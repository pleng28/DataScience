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
bc <- read.table("~/Desktop/DS 4559 2017/BreastCancerData.csv",sep = ",", header = TRUE)

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
bc$Bare.Nuclei[bc$Bare.Nuclei=="?"] =NA
bc$Bare.Nuclei <- as.numeric(bc$Bare.Nuclei)

## Now do things look better?
summary(bc)

## What to do with NAs?
sum(is.na(bc))

## Because there are not many NAs, one option is to remove all samples that contain any NAs.
bc <- bc[complete.cases(bc), ]

## We require one final fix to our data.  The class is being read as a numerical value, and since we are
## interested in classification, we need to fix this.

bc$Class <- as.factor(bc$Class)

## The current level labels are not very clear to us.  Let's make them meaningful.
levels(bc$Class)
levels(bc$Class) <- c("benign","malignant")

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
library(tree)
plot(t1)
text(t1,pretty=0)
adaboost$weights
adaboost$importance

## We can also study the error evolution as boosting moved through each of the 20 weak learners.
errorevol(adaboost,bc_train)


## Now we can make a prediction!

p <- predict(adaboost,bc_test)

## Notice p has multiple features (because "predict" in the adabag package is different than base "predict")
## Look at these features.

## We can study our performance using our classic confusion matrix summary:
CrossTable(bc_test$Class, p$class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diagnosis', 'predicted diagnosis'))

## We can also use ROC curves.  How is the ROC curve created?

roc(bc_test$Class,p$prob[2])
plot(RC, legacy.axes = TRUE)

## Now compare your performance with boosting to random forest performance, using the traditional 
## test/train approach (why this approach?)
## NOTE: you will need to add an argument to your predict statement in order to get probabilities rather
## than classification when you create your ROC curve and calculate AUC!!!  
## You should be able to Google your way through this process.