## A little midterm practice

install.packages('gapminder')
library(gapminder)
str(gapminder)
summary(gapminder)

## Think about the libraries you will need to complete the following, and library them.

## Create a tibble that displays only country, year, pop, gdpPercap for Cambodia.

## <YOUR CODE HERE>

##install.packages("tidyverse")
library(tidyverse)
Cambodia<-gapminder%>%mutate(continent=NULL,lifeExp=NULL,country="Cambodia")

as_tibble(Cambodia)



## Add a column for gdp to this tibble:

## <YOUR CODE HERE>
Cambodia%>%as_tibble()%>%mutate(
  gdp=pop*gdpPercap)

View(Cambodia)
## Now calculate the mean gdp across all years.

## <YOUR CODE HERE>

gapminder %>%
  filter(country == "Cambodia") %>%
  select(-continent, -lifeExp) %>%
  mutate(gdp = pop * gdpPercap) %>%
  group_by(country) %>%
  summarize(mean_gdp = mean(gdp)) %>%
  ungroup()

## Find the maximum life expectancy for countries in Asia.

## <YOUR CODE HERE>
life<-gapminder %>%
  filter(continent == "Asia") %>%
  select(-pop, -gdpPercap, -year) %>%
  group_by(country) %>%
  mutate(highestlife = max(lifeExp)) %>%
  ungroup()
View(life)


## For the simple iris dataset, create a decision tree and compare predictions to actual outcomes (Species).  
## Do the same for bagging and random forests, in each case using a cross table and calculating the accuracy 
## and reporting it.  In the case of bagging and rfs, also calculate the oob error on the full set of data
## and compare to the accuracy calculated for the traditional test set-training set approach.  Finally, determine
## variable importance for the variables in the iris dataset.  Explain how variable importance is determined.



