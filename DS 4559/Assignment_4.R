############## Assignment #4 ######################################################
## Read in the Pima_Indians_Data.txt.
## (1) (10 pts.) Clean the data (i.e., handle unreasonable values and missing data)
##               and create a training set and a test set using the traditional validation
##               set approach we have been using to date in class.

##


library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)
library(adabag)

pima <- read_csv("pima.csv",col_names=c("Pregnant","Plasma_Glucose","Dias_BP","Triceps_Skin","Serum_Insulin","BMI","DPF","Age","Diabetes"))
sapply(pima, function(x) sum(is.na(x)))
pima <- pima[apply(pima[,c(2,3,6)],1,function(x) !any(x==0)),]

##
##pima<- pima[!(rowSums(is.na(final))),]
##pima<- pima[!(rowSums(is.na(final))),]
##pima<- pima[!(rowSums(is.na(final))),]


summary(pima)


## (2) (10 pts.) Make yourself familiar with the problem and the predictor variables.  What are
##               we trying to detect?  What do the predictors Plasma.glucose.concentration
##               and Diabetes.pedigree.function have to do with the outcome variable?


gluc_mean <- pima %>% group_by(Diabetes) %>% summarise(Plas = round(mean(Plasma_Glucose),2))

#Relationship between Plasma Glucose & Diabetes
ggplot(data=pima,aes(Diabetes,Plasma_Glucose)) + 
  geom_boxplot(aes(fill=Diabetes)) + stat_boxplot(geom = "errorbar") + 
  ggtitle("Diabetes rates against Plasma Glucose Levels") + 
  xlab("Diabetes") + ylab("Plasma Glucose") + guides(fill=F) + 
  geom_text(data = gluc_mean, aes(x=Diabetes,y=Plas,label=Plas),
            hjust = -1.5,vjust=-0.5)

## (3) (10 pts.) Change the class variable from 0 and 1 to "no" and "diabetic." (Make sure these
##                are factors!)

pima$Class <- as.factor(pima$Class)
pima%>%
  mutate(Diabetes=case_when(
    .$Diabetes==1 ~ "diabetic",
    .$Diabetes==0 ~ "no"
  ))


##


## (4) (10 pts.) Run the boosting algorithm from "adabag" to train the model.  Use 50 trees.


summary(pima)
nrow(pima)
set.seed(123)
bc_rand <- pima[order(runif(768)), ]

# split the data frames
bc_train <- bc_rand[1:550, ]
bc_test  <- bc_rand[551:768, ]
str(bc_train)

pima$Class <- as.factor(pima$Class)

adaboost<-boosting(Diabetes ~. , data=bc_train, boos=FALSE, mfinal=50,coeflearn='Freund')
summary(adaboost)

barplot(bc_train$imp[order(bc_train$imp, decreasing = TRUE)], ylim = c(0, 100), main = "Boosting Relative Importance", col = "lightblue")
##

## (5) (10 pts.) What do the "weights" represent?  What are the three most important features
##               for detecting diabetes in this population?

## The probabilities columns in the boostin object are calculated by the adaboost function using weighted votes divieded by the sum of the weights
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)
pima$Diabetes <- as.factor(pima$Diabetes)
set.seed(15689)
m_dt <- tree(Diabetes ~ ., data = bc_train)
pred_dt <- predict(m_dt, bc_train, type = "class")
confusionMatrix(bc_train$Diabetes,pred_dt)[2:3]
plot(m_dt)
text(m_dt, pretty = 0)
pred_dt_test <- predict(m_dt, test, type = "class")
confusionMatrix(test$Diabetes,pred_dt_test)

acc_dt <- confusionMatrix(pred_dt_test,test$Diabetes)$overall['Accuracy']

#Random Forest
set.seed(15689)

opt_mod <- tuneRF(bc_train[-as.numeric(ncol(bc_train))],bc_train$Diabetes,ntreeTry = 150, 
                  stepFactor = 2, improve = 0.05,trace = T, plot = T, doBest = F)
plot(m_dt)
text(m_dt, pretty = 0)


pred_dt_test <- predict(m_dt, test, type = "class")
confusionMatrix(test$Diabetes,pred_dt_test)

acc_dt <- confusionMatrix(pred_dt_test,test$Diabetes)$overall['Accuracy']

#Random Forest
set.seed(15689)

opt_mod <- tuneRF(train[-as.numeric(ncol(train))],train$Diabetes,ntreeTry = 150, 
                  stepFactor = 2, improve = 0.05,trace = T, plot = T, doBest = F)
mtry_fin <- opt_mod[as.numeric(which.min(opt_mod[,"OOBError"])),"mtry"]

rf_fin <- randomForest(Diabetes~.,data=train, mtry=mtry_fin, ntree=101, 
                       keep.forest=TRUE, proximity=TRUE, importance=TRUE,test=test)

pred_test <- predict(rf_fin, newdata = test)
confusionMatrix(test$Diabetes,pred_test)

acc_rf <- confusionMatrix(test$Diabetes,pred_test)$overall['Accuracy']

par(mfrow=c(1,2))
varImpPlot(rf_fin,type = 2,main = "Variable Importance",col = 'black')
plot(rf_fin,main = "Error vs no. of trees grown")



## The three most important features for detecting diabetes in this population are Plasma Glucose, BMI and Age


## (6) (15 pts.) Make a prediction on the test data and report your performance in terms of 
##               area under the curve and a plotted ROC curve.

acc_rf <- confusionMatrix(test$Diabetes,pred_test)$overall['Accuracy']


plot.roc(bc_test$Diabetes,fin_pred,percent=TRUE,col="#1c61b6", print.auc=TRUE,
         main = "ROC Curve for Random Forest")


## A prediction on the test data is that Plasma Glucose means a high probability for diabeties. According to the ROC Curve, the prediction was correct. 

