# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The data is initially in a zip file. Here it is unzipped and put into environment as data.frame.

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
```

Now lets aggregate data and calculate total number of steps taken per day.

```r
totals <- aggregate(steps ~ date, data=activity, FUN=sum)
```

## What is mean total number of steps taken per day?
This histogram shows total number of steps taken per day.

```r
hist(totals$steps, main="Histogram of number of steps per day", xlab="Number of steps a day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Lets calculate mean for the total number of steps...

```r
meanSteps <- mean(totals$steps)
meanSteps
```

```
## [1] 10766.19
```

...and median for the total number of steps;

```r
medianSteps <- median(totals$steps)
medianSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Lets first calculate averages (means) for each interval across all days.

```r
intervalMeans <- aggregate(steps ~ interval, data = activity, FUN = mean)
```

Now we can plot average number of steps taken per interval.

```r
plot.ts(x=intervalMeans$interval, y=intervalMeans$steps, type = "l", main="Average number of steps taken per time interval", xlab="Time interval", ylab="Average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

You can see on a graph that there is a clear maximum for one of the intervals. Lets find that interval.

```r
intervalMeans[intervalMeans$steps==max(intervalMeans$steps),"interval"]
```

```
## [1] 835
```

## Imputing missing values
There is missing data in the dataset. Total number of rows with NAs is 2304 and all of those NAs are in the "steps" column.

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Let's create a new dataset, but replace NAs with interval mean calculated across all days.

```r
newActivity <- activity
for (i in 1:nrow(newActivity)) {
    if (is.na(newActivity[i,"steps"])) {
        newActivity[i,"steps"] <- intervalMeans[intervalMeans$interval == newActivity[i,"interval"],"steps"]
    }
}
```

Now we can build a histogram of total steps per day.

```r
newTotals <- aggregate(steps ~ date, data=newActivity, FUN=sum)
hist(newTotals$steps, main="Histogram of number of steps per day, corrected for missing values", xlab="Number of steps a day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

We can also calculate mean and median values for the new dataset and compare them against mean and median for the old (uncorrected for NAs) dataset.

```r
newMeanSteps <- mean(newTotals$steps)
newMedianSteps <- median(newTotals$steps)
print(paste("old mean - new mean =", meanSteps - newMeanSteps))
```

```
## [1] "old mean - new mean = 0"
```

```r
print(paste("old median - new median =", medianSteps - newMedianSteps))
```

```
## [1] "old median - new median = -1.1886792452824"
```
You can see that daily median got bigger by replacing missing values in the original dataset by interval means. Daily mean did not shift.

## Are there differences in activity patterns between weekdays and weekends?
First lets add to a dataset a new column that will show whether that day was a weekend or not.

```r
newActivity[, 'weekend'] <- weekdays(as.POSIXct(newActivity$date)) %in% c('Saturday', 'Sunday')
newIntervalMeansWeekday <- aggregate(steps ~ interval, data = newActivity[newActivity$weekend == FALSE,], FUN = mean)
newIntervalMeansWeekend <- aggregate(steps ~ interval, data = newActivity[newActivity$weekend == TRUE,], FUN = mean)
par(mfrow=c(2,1))
plot.ts(x=newIntervalMeansWeekend$interval, y=newIntervalMeansWeekend$steps, type = "l", main="Average number of steps taken per time interval on weekends", xlab="Time interval", ylab="Average number of steps")
plot.ts(x=newIntervalMeansWeekend$interval, y=newIntervalMeansWeekday$steps, type = "l", main="Average number of steps taken per time interval on weekdays", xlab="Time interval", ylab="Average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

As you can see on graphs above, a test subject started and finished moving earlier on weekdays, than on weekend. At the same time there was more activity overall on weekends.
