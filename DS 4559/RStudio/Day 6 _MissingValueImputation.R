############ DEALING WITH MISSING VALUES ###################################

## For the purposes of practice and example we modify the airquality dataset to
## have missing values.  You do not need to know how to do this on your own if you
## are uncomfortable with the notation:

data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA
data <- data[-c(5,6)]
summary(data)

## The following function calculates the percent of data that is missing.  You
## do not need to know how to write a function.  You may simply use it for future
## purposes:
pMiss <- function(x){sum(is.na(x))/length(x)*100}
## Now we apply the function to the columns (features) (that is what the '2' represents)
apply(data,2,pMiss)

## Next we apply the function to the rows (samples) (that is what the '1' indicates
apply(data,1,pMiss)

## Condsider removing some rows or columns with lots of missing data.
keepers <- which(apply(data,1,pMiss)<100)
keepers
data2 <- data[keepers,]
nrow(data2)

##  Now we need to check to see if there is a pattern in the missing values:
install.packages('Amelia')
library(Amelia)
missmap(data2)




## We can try something called "overall mean imputation:"

data2 %>% mutate(Ozone,ifelse(is.na(Ozone),mean(Ozone,na.rm = TRUE),Ozone))
## Continue the above command so that every variable has NAs imputed.

## We can try kNN imputation (from the DMwR package)
install.packages("DMwR")
library(DMwR)

data2_kNN <- knnImputation(data2, k=10)


##To deal with this data using multiple imputation, we use the mice package:

install.packages("mice")
library(mice)
tempData <- mice(data2,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

completedData <- complete(tempData,1)
summary(completedData)
