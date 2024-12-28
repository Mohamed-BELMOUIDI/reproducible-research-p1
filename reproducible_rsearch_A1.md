## 1. Introduction

In this analysis, we will explore the activity dataset, focusing on the
total number of steps taken per day, average daily activity patterns,
and differences in activity between weekdays and weekends. The dataset
contains step counts recorded in 5-minute intervals across several days,
with some missing values that we will handle appropriately.

## 2. Loading and Preprocessing the Data

First, we download and unzip the dataset, then load it into R for
further analysis.

    # Download the activity data file
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip")

    # Unzip the downloaded file
    unzip(zipfile = "activity.zip")

    # Read the dataset into R
    data <- read.csv("activity.csv")

## 3. Total Number of Steps Taken Per Day

We now calculate the total number of steps taken for each day and
visualize it with a histogram. Additionally, we compute the mean and
median total steps across all days.

    library(ggplot2)

    # Calculate total steps taken each day
    total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

    # Create a histogram of total steps per day
    qplot(total.steps, binwidth=1000, xlab="Total Number of Steps Taken Each Day")

    ## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](reproducible_rsearch_A1_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    # Calculate mean and median of total steps
    mean(total.steps, na.rm=TRUE)

    ## [1] 9354.23

    median(total.steps, na.rm=TRUE)

    ## [1] 10395

## 4. Average Daily Activity Pattern

Next, we examine the average number of steps taken during each 5-minute
interval across all days. This will help us understand the general
activity pattern.

    # Calculate the average number of steps for each 5-minute interval
    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                          FUN=mean, na.rm=TRUE)

    # Plot the average activity pattern over time
    ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute Interval") +
        ylab("Average Number of Steps Taken")

![](reproducible_rsearch_A1_files/figure-markdown_strict/unnamed-chunk-2-1.png)

To identify the time period with the highest activity, we find the
interval with the maximum number of average steps.

    # Find the 5-minute interval with the maximum number of steps
    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

## 5. Handling Missing Data

The dataset contains missing values (coded as `NA`). We will address
these by imputing the missing step counts with the average value for
each 5-minute interval. This ensures that we do not introduce bias due
to missing data.

    # Identify missing values in the dataset
    missing <- is.na(data$steps)

    # Count the number of missing values
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

We then define a function to replace each missing step count with the
corresponding interval’s average steps.

    # Function to impute missing steps with the mean value for the 5-minute interval
    fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
            filled <- c(steps)
        else
            filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
    }

    # Apply the function to fill missing values in the dataset
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

### Total Steps After Imputation

Now, let’s compute the total steps taken per day using the filled data
and visualize the results with a histogram. We also calculate the mean
and median of the total steps after imputing the missing values.

    # Calculate total steps taken each day in the filled dataset
    total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)

    # Create a histogram of total steps per day after imputing missing values
    qplot(total.steps, binwidth=1000, xlab="Total Number of Steps Taken Each Day")

![](reproducible_rsearch_A1_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    # Calculate mean and median of total steps after imputation
    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

By imputing missing values, the mean and median steps are higher
compared to the original data. This is because days with missing values
were initially counted as zero steps, but after imputing, the average
steps for those intervals are restored.

## 6. Weekday vs. Weekend Activity Patterns

Finally, we analyze whether there are any differences in activity
patterns between weekdays and weekends. We start by classifying each day
as a weekday or weekend.

    # Function to classify a day as weekday or weekend
    weekday.or.weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
            return("weekday")
        else if (day %in% c("Saturday", "Sunday"))
            return("weekend")
        else
            stop("Invalid date")
    }

    # Convert the 'date' column to Date type and classify each day
    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

We now aggregate the steps by day and interval, and plot the results to
compare activity on weekdays and weekends.

    # Aggregate the steps data by interval and day
    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)

    # Create a panel plot comparing activity on weekdays and weekends
    ggplot(averages, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(day ~ .) +
        xlab("5-minute Interval") + 
        ylab("Number of Steps")

![](reproducible_rsearch_A1_files/figure-markdown_strict/unnamed-chunk-7-1.png)

## 7. Conclusion

Through this analysis, we have examined the total number of steps taken
per day, identified the average daily activity patterns, handled missing
values, and compared activity patterns between weekdays and weekends.
The findings show that weekday and weekend activity patterns differ, and
imputing missing data significantly impacts the results by removing zero
step days.
