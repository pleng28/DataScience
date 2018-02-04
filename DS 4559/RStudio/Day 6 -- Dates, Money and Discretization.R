### Here are some final useful bits of code to help you with various data transformations
### and common data problems.

## Dealing with dates:

# Below, I am creating a simple vector of dates (much like a comlumn in a dataframe):

dates <- c("05/27/84", "07/07/05")

# Now I use the as.Date function to change the given format to a standard format.
betterDates <- as.Date(dates,format = "%m/%d/%y")

# Let's check the output:
betterDates

# Below, I do the same thing as above, but this time I am converting from another format

# Here, again, I make a simple vector for the purposes of demonstration
dates <- c("May 27 1984", "July 7 2005")

# I run my conversion
betterDates <- as.Date(dates,
                       format = "%B %d %Y")
# And I check my results:
betterDates

## Note, R now recognizes these dates as dates and can perform operations on them:

min(betterDates)
max(betterDates)
mean(betterDates)

###### MAKING CURRENCY NUMERIC ##############################################
## Below, I am simply creating a simple feature vector.
money <- c('$200,000', '$400,000')

## This command transforms currency strings into numeric values.
money <- as.numeric(gsub('[$,]', '', money))

## Let's check that it worked:
money

## Notice that now R allows me to operate on the money as numeric values:
mean(money)


###### DESCRETIZING A VARIABLE(FEATURE) ############################

age <- c(23,45,40,29,46,56,55,53,29,30,35,32,41)
age_disc <- cut_width(age,10,center = 25)

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

