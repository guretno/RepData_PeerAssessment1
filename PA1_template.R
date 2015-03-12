library(lattice)
library(knitr)
library(xtable)

## Loading and preprocessing the data

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "./repdata-data-activity/activity.csv")
unzip("repdata-data-activity.zip", exdir = "./repdata-data-activity")

activity_data <- read.csv("./repdata-data-activity/activity.csv")

filter_steps <- activity_data$steps[!is.na(activity_data$steps)]
filter_date <- activity_data$date[!is.na(activity_data$steps)]


## What is mean total number of steps taken per day?

# get the total number of steps for each day
total_steps <- tapply(filter_steps, factor(filter_date), FUN = sum)

# plot a histogram of the total number of steps taken each day
histogram(total_steps, breaks = 10, 
          xlab = "Total number of steps per day", 
          main = "Distribution of total steps per day", 
          col = "lightblue", 
          type = "count")

# mean and median of total steps
mean_total_steps <- mean(total_steps)
median_total_steps <- median(total_steps)

## What is the average daily activity pattern?

interval <- activity_data$interval

# calculate the average number of steps for each 5 minute period
average_steps <- tapply(activity_data$steps, factor(interval), FUN = mean, na.rm = TRUE)
average_steps <- sapply(average_steps, simplify = array, round, 2)
    
   
# plot the time series
xyplot(as.numeric(average_steps) ~ interval[1:288], 
       type = "l", 
       xlab = "Time interval",
       ylab = "Average steps", 
       main = "Time series - average steps vs time interval", 
       scales = list( x=list(at = seq(0, 2400, 200))) )


# create a data frame of average steps and time interval
interval_factor <- factor(interval)[1:nlevels(factor(interval))]
df_steps_interval <- data.frame(interval_factor, average_steps)

# sort df to get the row with the maximum amount of average steps
df_steps_interval <- df_steps_interval[order(df_steps_interval$average_steps, 
                                             decreasing = TRUE),]

# convert the factor to a character and then to numeric
max_time_interval <- as.numeric(as.character(df_steps_interval$interval_factor[1]))

## Imputing missing values

steps <- activity_data$steps

# Calculate and report the total number of missing values in the dataset
length(steps[is.na(steps)])

# take a copy of the original steps vector
new_dataset_steps <- steps

# Devise a strategy for filling in all of the missing values in the dataset
for (i in which(sapply(new_dataset_steps, is.na))) {
  
  # set the value to the equivalent value in the average vector
  if (i <= 288){
    new_dataset_steps[i] <- average_steps[i]
  } 
  
  # wrap around 288 (avg time only has 24 hours of data) and add one because 
  # R is non-zero index
  else{
    new_dataset_steps[i] <- average_steps[i%%288 + 1]
  }
  
}

#Create a new dataset that is equal to the original dataset but with the missing data filled in
new_activity_data <- activity_data
new_activity_data$steps <- new_dataset_steps


# get the total number of steps for each day
new_dataset_total_steps <- tapply(new_dataset_steps, factor(new_dataset_steps), FUN = sum)

# plot a histogram of the total number of steps taken each day
histogram(new_dataset_total_steps, breaks = 10, 
          xlab = "Total number of steps per day", 
          main = "Distribution of total steps per day after imputted values", 
          col = "lightblue",
          type = "count")

# calculate the mean and median of the distribution
new_dataset_mean <- mean(new_dataset_total_steps)
new_dataset_median <- median(new_dataset_total_steps)


original_summary <- c(mean_total_steps, median_total_steps)
new_summary <- c(new_dataset_mean, new_dataset_median)
summary <- data.frame(original_summary, new_summary)
rownames(summary)<-c("mean", "median")
xtable(summary)


## Are there differences in activity patterns between weekdays and weekends?


date <- as.Date(activity_data$date)
day_of_the_week <- weekdays(date)
weekendDays <- c("Saturday", "Sunday")

# construct a DF for these 4 values
DF <- data.frame(date, interval_factor, new_dataset_steps, day_of_the_week)
isWeekend <- DF$day_of_the_week %in% weekendDays
DF$dayType = factor(isWeekend,labels = c("Weekday","Weekend"))

# plot the time series
xyplot(DF$new_dataset_steps ~ interval | DF$dayType, layout = c(2, 1), type = "l", 
       xlab = "Time interval", ylab = "Number of steps", 
       main = "Time series of numer of steps vs time interval" )