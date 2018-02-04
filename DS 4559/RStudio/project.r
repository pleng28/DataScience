prepare.package <- function(package) {
  if(package %in% rownames(installed.packages()) == FALSE) {
    install.packages(package)
  }
  require(package, character.only = TRUE)
}

setwd(dirname(parent.frame(2)$ofile))
prepare.package("doBy")
prepare.package("ggplot2")
prepare.package("gdata")
prepare.package("plyr")

################################################################################
# First challenge: load in and clean up the data
################################################################################


mh <- read.csv(file="manhattan.csv",head=TRUE,sep=",")


# Get some information about what we have to work with
head(mh)
summary(mh)

# Creates a numerical version of columns containing commas and/or dollar signs
mh$SALE.PRICE.n <- as.numeric(gsub("[^[:digit:]]", "", mh$SALE.PRICE))
mh$GROSS.SQUARE.FEET <- as.numeric(gsub("[^[:digit:]]", "", mh$GROSS.SQUARE.FEET))
mh$LAND.SQUARE.FEET <- as.numeric(gsub("[^[:digit:]]", "", mh$LAND.SQUARE.FEET))

# See if we have any invalid values within the newly created columns
count(is.na(mh$SALE.PRICE.n))
count(is.na(mh$GROSS.SQUARE.FEET))
count(is.na(mh$LAND.SQUARE.FEET))

################################################################################
# Next, conduct exploratory data analysis in order to find out where there are 
# outlier or missing values, decide how you will treat them, make sure the 
# dates are formatted correctly, make sure values you think are numerical are 
# being treated as such, etc.
################################################################################

# Making sure dates are formatted correctly
mh$SALE.DATE <- as.Date(mh$SALE.DATE)
# Making sure values I think are numerical are being treated as such
mh$YEAR.BUILT <- as.numeric(as.character(mh$YEAR.BUILT))

# Some exploration to make sure there's nothing weird going on with sale prices

# Total sales on record
ggplot(mh, aes(x=SALE.PRICE.n)) + 
  geom_histogram(binwidth = diff(range(mh$SALE.PRICE.n)))
# Total sales on record with a price more than 0
ggplot(subset(mh, SALE.PRICE.n>0), aes(x=SALE.PRICE.n)) + 
  geom_histogram(binwidth = diff(range(mh$SALE.PRICE.n)))
# Total sales on record with a price of 0
ggplot(subset(mh, SALE.PRICE.n==0), aes(x=GROSS.SQUARE.FEET)) + 
  geom_histogram(binwidth = diff(range(mh$GROSS.SQUARE.FEET)))

# We shall treat sales with price of 0 as missing values and omit them

# Keep only the actual sales
mh.sale <- mh[mh$SALE.PRICE.n!=0,]

# Let us look for some outliers
ggplot(mh.sale, aes(GROSS.SQUARE.FEET, SALE.PRICE.n)) + geom_point()
ggplot(mh.sale, aes(log(GROSS.SQUARE.FEET), log(SALE.PRICE.n))) + geom_point()

# Seems like we found some let's investigate further and see what we can do

# Factorize BUILDING.CLASS.CATEGORY
mh.sale$BUILDING.CLASS.CATEGORY <- factor(mh.sale$BUILDING.CLASS.CATEGORY)

# Let us look at building types and see what is most reasonable to work with
levels(mh.sale$BUILDING.CLASS.CATEGORY)

# We do want to work with building types that give us GROSS.SQUARE.FEET and lad.sqft
summaryBy(GROSS.SQUARE.FEET+LAND.SQUARE.FEET~BUILDING.CLASS.CATEGORY, 
          data=mh.sale, FUN=c(min, max))

# It seems most reasonable to work with 1, 2, and 3 family homes
mh.homes <- mh.sale[which(grepl("FAMILY",mh.sale$BUILDING.CLASS.CATEGORY)),]

# Let us look and see if we still have outliers
ggplot(mh.homes, aes(GROSS.SQUARE.FEET, SALE.PRICE.n)) + geom_point()
ggplot(mh.homes, aes(log(GROSS.SQUARE.FEET), log(SALE.PRICE.n))) + geom_point()

# We seem to have some, let's take a closer look
summaryBy(SALE.PRICE.n~ADDRESS, data=subset(mh.homes, SALE.PRICE.n<10000), 
          FUN=c(length, min, max))

# The summarized values above are definitely outliers and so we shall omit them
mh.homes$outliers <- (log(mh.homes$SALE.PRICE.n) <=5) + 0
mh.homes <- mh.homes[which(mh.homes$outliers==0),]

# Let us look and see if we still have outliers
ggplot(mh.homes, aes(GROSS.SQUARE.FEET, SALE.PRICE.n)) + geom_point()
ggplot(mh.homes, aes(log(GROSS.SQUARE.FEET), log(SALE.PRICE.n))) + geom_point()

# This looks beautiful, we can now continue with our work!

################################################################################
# Once the data is in good shape, conduct exploratory data analysis to visualize
# and make compariosons (i) across NEIGHBORHOODs,
################################################################################

# Factorize NEIGHBORHOOD
mh.homes$NEIGHBORHOOD <- factor(mh.homes$NEIGHBORHOOD)

# Let's first see which NEIGHBORHOODs are included in our data
levels(mh.homes$NEIGHBORHOOD)

# Let's explore the data quantitatively first:

metrics <- function(x) {
  c(length(x),                 # count
    round(mean(x)),            # mean
    round(median(x)),          # median
    names(sort(-table(x)))[1], # mode
    round(sd(x)),              # standard deviation
    min(x),                    # minimum
    max(x),                    # maximum
    max(x)-min(x))             # range
}

# Sale price across NEIGHBORHOODs
summaryBy(SALE.PRICE.n~NEIGHBORHOOD, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Gross square feet across NEIGHBORHOODs
summaryBy(GROSS.SQUARE.FEET~NEIGHBORHOOD, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Land square feet across NEIGHBORHOODs
summaryBy(LAND.SQUARE.FEET~NEIGHBORHOOD, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Year built  across NEIGHBORHOODs
summaryBy(YEAR.BUILT~NEIGHBORHOOD, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))

# Let's now explore the data by visualizing it:

# Number of sales of homes across NEIGHBORHOODs
ggplot(mh.homes, aes(x=NEIGHBORHOOD, fill=NEIGHBORHOOD, ymax=max(..count..))) + 
  geom_histogram(binwidth = diff(range(mh.homes$SALE.PRICE.n)/28)) +
  stat_bin(binwidth=1,geom="text",drop=TRUE,aes(label=..count.., vjust=-1))

# Sale price across NEIGHBORHOODs
# Histogram
ggplot(mh.homes, aes(x=SALE.PRICE.n, fill=NEIGHBORHOOD)) + 
  geom_histogram(binwidth = diff(range(mh.homes$SALE.PRICE.n)/60))
# Box and whisker plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=SALE.PRICE.n, fill=NEIGHBORHOOD)) + 
  geom_boxplot() + coord_flip()
# Density plot
ggplot(mh.homes, aes(x=SALE.PRICE.n, color=NEIGHBORHOOD)) + geom_density()
# Jitter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=SALE.PRICE.n, color=NEIGHBORHOOD)) + 
  geom_jitter()
# Scatter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, SALE.PRICE.n, color=NEIGHBORHOOD)) + 
  geom_point() + coord_flip()
# Kernel density estimate
ggplot(mh.homes, aes(x=SALE.PRICE.n, fill=NEIGHBORHOOD)) + stat_density()

# Gross square feet across NEIGHBORHOODs
# Histogram
ggplot(mh.homes, aes(x=GROSS.SQUARE.FEET, fill=NEIGHBORHOOD)) + 
  geom_histogram(binwidth = diff(range(mh.homes$GROSS.SQUARE.FEET)/60)) + coord_flip()
# Box and whisker plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=GROSS.SQUARE.FEET, fill=NEIGHBORHOOD)) + 
  geom_boxplot() + coord_flip()
# Density plot
ggplot(mh.homes, aes(x=GROSS.SQUARE.FEET, color=NEIGHBORHOOD)) + geom_density()
# Jitter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=GROSS.SQUARE.FEET, color=NEIGHBORHOOD)) + 
  geom_jitter()
# Scatter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, GROSS.SQUARE.FEET, color=NEIGHBORHOOD)) + 
  geom_point() + coord_flip()
# Kernel density estimate
ggplot(mh.homes, aes(x=GROSS.SQUARE.FEET, fill=NEIGHBORHOOD)) + stat_density()

# Land square feet across NEIGHBORHOODs
# Histogram
manxlim <- mh.homes%>%filter(LAND.SQUARE.FEET<5000)
ggplot(manxlim, aes(x=LAND.SQUARE.FEET, fill=NEIGHBORHOOD)) + 
  geom_histogram(binwidth = diff(range(mh.homes$LAND.SQUARE.FEET)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=LAND.SQUARE.FEET, fill=NEIGHBORHOOD)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=LAND.SQUARE.FEET, color=NEIGHBORHOOD)) + geom_density()
# Jitter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=LAND.SQUARE.FEET, color=NEIGHBORHOOD)) + 
  geom_jitter()
# Scatter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, LAND.SQUARE.FEET, color=NEIGHBORHOOD)) + 
  geom_point()
# Kernel density estimate
ggplot(mh.homes, aes(x=LAND.SQUARE.FEET, fill=NEIGHBORHOOD)) + stat_density()

# Land square feet across NEIGHBORHOODs
# Histogram
ggplot(mh.homes, aes(x=LAND.SQUARE.FEET, fill=NEIGHBORHOOD)) + 
  geom_histogram(binwidth = diff(range(mh.homes$LAND.SQUARE.FEET)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=LAND.SQUARE.FEET, fill=NEIGHBORHOOD)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=LAND.SQUARE.FEET, color=NEIGHBORHOOD)) + geom_density()
# Jitter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=LAND.SQUARE.FEET, color=NEIGHBORHOOD)) + 
  geom_jitter()
# Scatter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, LAND.SQUARE.FEET, color=NEIGHBORHOOD)) + 
  geom_point()
# Kernel density estimate
ggplot(mh.homes, aes(x=LAND.SQUARE.FEET, fill=NEIGHBORHOOD)) + stat_density()

# Year built across NEIGHBORHOODs
# Histogram
ggplot(mh.homes, aes(x=YEAR.BUILT, fill=NEIGHBORHOOD)) + 
  geom_histogram(binwidth = diff(range(mh.homes$YEAR.BUILT)/60))
# Box and whisker plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=YEAR.BUILT, fill=NEIGHBORHOOD)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=YEAR.BUILT, color=NEIGHBORHOOD)) + geom_density()
# Jitter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, y=YEAR.BUILT, color=NEIGHBORHOOD)) + 
  geom_jitter()
# Scatter Plot
ggplot(mh.homes, aes(x=NEIGHBORHOOD, YEAR.BUILT, color=NEIGHBORHOOD)) + 
  geom_point()
# Kernel density estimate
ggplot(mh.homes, aes(x=YEAR.BUILT, fill=NEIGHBORHOOD)) + stat_density() + coord_flip()

################################################################################
# and (ii) across time.
################################################################################

# We only have data for August, 2012 - August 2013
# Seeing as we don't have full years or comparable months of those years
# it would be wiser to conduct EDA across months and weekdays only.

# Categorize sale dates by month and weekday
mh.homes$sale.month <- format(mh.homes$SALE.DATE, "%B")
mh.homes$sale.day <- format(mh.homes$SALE.DATE, "%A")

# Factorize sale dates by month and weekday
mh.homes$sale.month <- 
  factor(mh.homes$sale.month, 
         levels= c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November",
                   "December"))
mh.homes$sale.day <- 
  factor(mh.homes$sale.day, 
         levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

# Let's explore the data quantitatively first:

# Sale price across months
summaryBy(SALE.PRICE.n~sale.month, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Gross square feet across months
summaryBy(gross.sqft~sale.month, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Land square feet across months
summaryBy(LAND.SQUARE.FEET~sale.month, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Year built across months
summaryBy(YEAR.BUILT~sale.month, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))

# Sale price across days of the week
summaryBy(SALE.PRICE.n~sale.day, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Gross square feet across days of the week
summaryBy(gross.sqft~sale.day, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Land square feet across days of the week
summaryBy(land.sqft~sale.day, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))
# Year built across days of the week
summaryBy(YEAR.BUILT~sale.day, data=mh.homes, FUN=metrics,
          fun.names=c("Count","Mean","Median","Mode","SD","Min","Max","Range"))

# Let's now explore the data by visualizing it:

# Number of sales of homes
# By month
ggplot(mh.homes, aes(x=sale.month, fill=sale.month, ymax=max(..count..))) + 
  geom_histogram(binwidth = diff(range(mh.homes$SALE.PRICE.n)/12)) +
  stat_bin(binwidth=1,geom="text",drop=TRUE,aes(label=..count.., vjust=-1))
# By day of the week
ggplot(mh.homes, aes(x=sale.day, fill=sale.day, ymax=max(..count..))) + 
  geom_histogram(binwidth = diff(range(mh.homes$SALE.PRICE.n)/7)) +
  stat_bin(binwidth=1,geom="text",drop=TRUE,aes(label=..count.., vjust=-1))

# Sale price across months
# Histogram
ggplot(mh.homes, aes(x=SALE.PRICE.n, fill=sale.month)) + 
  geom_histogram(binwidth = diff(range(mh.homes$SALE.PRICE.n)/60))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.month, y=SALE.PRICE.n, fill=sale.month)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=SALE.PRICE.n, color=sale.month)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.month, SALE.PRICE.n, color=sale.month)) + 
  geom_point()
# Sale price across days of the week
# Histogram
ggplot(mh.homes, aes(x=SALE.PRICE.n, fill=sale.day)) + 
  geom_histogram(binwidth = diff(range(mh.homes$SALE.PRICE.n)/60))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.day, y=SALE.PRICE.n, fill=sale.day)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=SALE.PRICE.n, color=sale.day)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.day, SALE.PRICE.n, color=sale.day)) + geom_point()

# Gross square feet (for sold homes) across months
# Histogram
ggplot(mh.homes, aes(x=gross.sqft, fill=sale.month)) + 
  geom_histogram(binwidth = diff(range(mh.homes$gross.sqft)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.month, y=gross.sqft, fill=sale.month)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=gross.sqft, color=sale.month)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.month, gross.sqft, color=sale.month)) + geom_point()
# Gross square feet (for sold homes) across days of the week
# Histogram
ggplot(mh.homes, aes(x=gross.sqft, fill=sale.day)) + 
  geom_histogram(binwidth = diff(range(mh.homes$gross.sqft)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.day, y=gross.sqft, fill=sale.day)) + geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=gross.sqft, color=sale.day)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.day, gross.sqft, color=sale.day)) + geom_point()

# Land square feet (for sold homes) across months
# Histogram
ggplot(mh.homes, aes(x=land.sqft, fill=sale.month)) + 
  geom_histogram(binwidth = diff(range(mh.homes$land.sqft)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.month, y=land.sqft, fill=sale.month)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=land.sqft, color=sale.month)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.month, land.sqft, color=sale.month)) + geom_point()
# Land square feet (for sold homes) across days of the week
# Histogram
ggplot(mh.homes, aes(x=land.sqft, fill=sale.day)) + 
  geom_histogram(binwidth = diff(range(mh.homes$land.sqft)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.day, y=land.sqft, fill=sale.day)) + geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=land.sqft, color=sale.day)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.day, land.sqft, color=sale.day)) + geom_point()

# Year built (for sold homes) across months
# Histogram
ggplot(mh.homes, aes(x=YEAR.BUILT, fill=sale.month)) + 
  geom_histogram(binwidth = diff(range(mh.homes$YEAR.BUILT)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.month, y=YEAR.BUILT, fill=sale.month)) + 
  geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=YEAR.BUILT, color=sale.month)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.month, YEAR.BUILT, color=sale.month)) + geom_point()
# Year built (for sold homes) across days of the week
# Histogram
ggplot(mh.homes, aes(x=YEAR.BUILT, fill=sale.day)) + 
  geom_histogram(binwidth = diff(range(mh.homes$YEAR.BUILT)/30))
# Box and whisker plot
ggplot(mh.homes, aes(x=sale.day, y=YEAR.BUILT, fill=sale.day)) + geom_boxplot()
# Density plot
ggplot(mh.homes, aes(x=YEAR.BUILT, color=sale.day)) + geom_density()
# Scatter Plot
ggplot(mh.homes, aes(x=sale.day, YEAR.BUILT, color=sale.day)) + geom_point()

################################################################################
# If you have time, start looking for meaningful patters in this dataset.
################################################################################

# - Most sales happen in Harlem and the Upper East Side.
# - Housing is most expensive in the Upper East Side.
# - Housing is least expensive in Harlem.
# - Houses have the most gross square feet in Gremich and the Upper East Side.
# - Houses seem about equal in terms of gross square feet elsewhere.
# - Houses seem about equal in terms of land square feet throughout.
# - The Upper West side has the newest housing.
# - Housing was built mostly around the early 1900s.
# - Harlem has the newest housing.
# - The amount of sales seem equally distributed around Wednesday.
# - December has a lot more sales than any other month.
# - The least amount of sales happen in September and October.
# - Home sales according to other metrics across time seem equally distributed.


