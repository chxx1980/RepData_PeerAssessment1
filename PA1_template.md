---
title: "Reproducible Research: Peer Assessment 1"
output: 
knit  html_document:
    keep_md: true
---

# Loading and preprocessing the data

Load the data (i.e. read.csv())  
Process/transform the data (if necessary) into a format suitable for your analysis  

```r
unzip(zipfile="activity.zip")
```

```
## Error in unzip(zipfile = "activity.zip"): cannot open file './activity.csv': Permission denied
```

```r
data <- read.csv("activity.csv")
```

# What is mean total number of steps taken per day?

Calculate the total number of steps taken per day  

```r
totalsteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  

```r
library(ggplot2)
qplot(totalsteps, binwidth = 800, xlab="Total number of steps taken per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day  

```r
mean(totalsteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalsteps, na.rm=TRUE)
```

```
## [1] 10395
```

# What is the average daily activity pattern?

```r
avgsteps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
```


Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  


```r
library(ggplot2)
ggplot(data=avgsteps, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken, averaged across all days")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
avgsteps[which.max(avgsteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

Peak activity is in the morning, at 8:35, average walking during the day, no walking at night. 

# Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
missing <- is.na(data$steps)
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (avgsteps[avgsteps$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
filledtotalsteps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
qplot(filledtotalsteps, binwidth = 800, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
mean(filledtotalsteps)
```

```
## [1] 10766.19
```

```r
median(filledtotalsteps)
```

```
## [1] 10766.19
```

After replacing the missing data with averages, the 0 counts are gone and the mean and median values are much higher.

# Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  

```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avgsteps <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(avgsteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

The main difference is that on weekends the peaks are less pronounced. More walking occures during daytime than on weekdays.
