# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
  
1. Load the data (i.e. read.csv())


```r
if (file.exists("activity.zip") & !file.exists("activity.csv"))
{
    unzip("activity.zip")
}

activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date, format = '%Y-%m-%d')
```

  
## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
stepsperday <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
head(stepsperday)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(stepsperday$steps, breaks = 35, col = "darkblue", main = "Total number of steps taken each day",
     xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsperday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsperday$steps)
```

```
## [1] 10765
```
  

## What is the average daily activity pattern?
  
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsavg <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
head(stepsavg)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(stepsavg$interval, stepsavg$steps, type = "l", col = "darkblue", main = "Time series: Average number
    of steps taken across all days", xlab = "5-minute Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsavg$interval[which.max(stepsavg$steps)]
```

```
## [1] 835
```

  
## Imputing missing values
  
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy applied was to assign the mean value of the 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
stepsnona <- activity

for (i in stepsavg$interval)
{
    stepsnona[stepsnona$interval == i & is.na(stepsnona$steps), ]$steps <-
        stepsavg$steps[stepsavg$interval == i]
}

head(stepsnona)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsperdaytotal <- aggregate(steps ~ date, data = stepsnona, sum)
hist(stepsperdaytotal$steps, breaks = 35, col = "darkblue", main = "Total number of steps taken each day
     (Imputed missing values)", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\

```r
mean(stepsperdaytotal$steps)
```

```
## [1] 10766.19
```

```r
median(stepsperdaytotal$steps)
```

```
## [1] 10766.19
```

The mean value did not change, but the median had a gone up slightly. Since the points imputed have the same values as the mean, the median is much likely to be the same as the mean.
  
## Are there differences in activity patterns between weekdays and weekends?
  
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(timeDate)

stepsnona$week <- weekdays(stepsnona$date)
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

stepsnona$dayweek <- as.factor(ifelse(is.element(weekdays(as.Date(stepsnona$date)), weekdays), "weekday", "weekend"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)

stepsnonaavg <- aggregate(steps ~ interval + dayweek, data = stepsnona, mean)

xyplot(steps ~ interval | dayweek, data = stepsnonaavg, type = "l", lwd = 1,
       layout = c(1, 2), xlab = "5-minute Interval", ylab = "Average number of steps",
       main = "Average Number of Steps Taken
       (across all weekday or weekend)")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)\
  
