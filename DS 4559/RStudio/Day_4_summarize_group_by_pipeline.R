#### Day4 -- Summarize, group)by(), pipelining and exploring data more closely

### Looking at summarize() and group_by() ##################

##Example:

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

##### Putting it all together with a pipe ##########################

## Here is some standard code you might write.  In it, we first group flights by destination, then summarize
## to compute distance, average delay, and number of flights, and finally filter to remove noisy points
## and Honolulu airport, which is almost twice as far away as the next closest airport.

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count >20, dest != "HNL")

## Now create a visualization that examines the relationship between delay and dist, showing points and
## the best fit curve through the points, and making the size of the points reflective of the number of
## flights:

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count)) +
  geom_smooth(se = FALSE)

## We can write this a different way using a "pipe" %>%

delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count = n(), 
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

## This way of expressing your commands, really lets you (and others) focus on the
## transformations being made.

## There are multiple commands/functions for the purposes of summarizing grouped data.  They include: 
## first(), last(), quantile(x, 0.25), max(x), min(x), sd(x), . . .

## Example:

## Let's make a dataframe that represents only flights that actually flew"
not_cancelled <- flights%>%
  filter(, )

## This is a good dataframe to keep for future use.  For example, to study delays of specific
## planes

delays<- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE)
  )

ggplot(data = delays, mapping = aes(x = delay)) + geom_freqpoly(binwidth = 10)
  

### Looks like there are flights that are on average 5hrs delayed!  Let's take a closer look:

delays<- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
    
  ggplot(data = delays, mapping = aes(x=n, y=delay)) + geom_point(alpha = 1/10)

## Zooming in:
  
  delays %>%
    filter(n > 25) %>%
    ggplot(mapping = aes(x = n, y = delay)) + geom_point(alpha=1/10)

### You've already seen us use the count function n().  There is a useful variation of this command,
  # known as the n_distinct() command
  # To answer the question, which destinations have the most carriers, this will be a useful function:
  
  not_cancelled  %>%
    group_by(dest) %>%
    summarize(carriers = n_distinct(carrier)) %>% 
    arrange(desc(carriers))

 ## Side note: quick and easy count check:
  
  not_cancelled %>%
    count(dest)

## Basically counts are so useful, R also offers the count() function.

  
## Finally, mutates and filters can also be used in conjunction with summarize to do useful things like:

## 1.  Find the worst member of each group:
  
  
flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay))<10)


## 2. Find all groups bigger than a threshold:

popular_dests <- flights %>%
  group_by(dest)  %>%
  filter(n() > 365)
popular_dests
             
## 3. Standardize to compute per group metrics:

popular_dests %>% filter(arr_delay > 0)%>%
  mutate(prop_delay = arr_delay/sum(arr_delay))%>%
  select (year:day, dest, arr_delay, prop_delay)
  
  
  
  
  
  
### Saving your dataframes: write.csv(<DATAFRAME_NAME>,<FILE_NAME>)
### NOTE: The <FILE_NAME> needs to include the path to the proper directory UNLESS you have specified your
### working directory.
  

  
  
  