---
##title: "Repro_Research"
#author: "Venkata Nagendra K Tadakamalla"
#date: "June 26, 2016"
#output: html_document
---



## Loading and preprocessing the data


```r
## 1. Load the data (i.e. read.csv())
Colcls = c("integer", "character", "integer")
datafile <- read.csv("C:/Users/tvnag/Documents/repdata_data_activity/activity.csv", head=TRUE, colClasses=Colcls, na.strings="NA")
head(datafile)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
## 2. Process/transform the data (if necessary) into a format suitable for your analysis
datafile$date <- as.Date(datafile$date)
datafile_ign <- subset(datafile, !is.na(datafile$steps))
```

## What is mean total number of steps taken per day?


```r
## Histogram of the daily total number of steps
dailysum <- tapply(datafile_ign$steps, datafile_ign$date, sum, na.rm=TRUE, simplify=T)
dailysum <- dailysum[!is.na(dailysum)]

hist(x=dailysum,
     col="red",
     breaks=20,
     xlab="Daily total steps",
     ylab="Frequency",
     main="The distribution of daily total (missing data ignored)")
```

![plot of chunk plothist1](figure/plothist1-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

```r
mean(dailysum)
```

```
## [1] 10766.19
```

```r
median(dailysum)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
int_avg <- tapply(datafile_ign$steps, datafile_ign$interval, mean, na.rm=TRUE, simplify=T)
datafile_ia <- data.frame(interval=as.integer(names(int_avg)), avg=int_avg)

with(datafile_ia,
     plot(interval,
          avg,
          type="l",
          xlab="5-minute intervals",
          ylab="average steps in the interval across all days"))
```

![plot of chunk plothist2](figure/plothist2-1.png)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_num_steps <- max(datafile_ia$avg)
datafile_ia[datafile_ia$avg == max_num_steps, ]
```

```
##     interval      avg
## 835      835 206.1698
```

## Imputing missing values
####  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(datafile$steps))
```

```
## [1] 2304
```
#### Devise a strategy for filling in all of the missing values in the dataset.

```r
## Create a new data frame
datafile_impute <- datafile
data_ndx <- is.na(datafile_impute$steps)
int_avg <- tapply(datafile_ign$steps, datafile_ign$interval, mean, na.rm=TRUE, simplify=T)
datafile_impute$steps[data_ndx] <- int_avg[as.character(datafile_impute$interval[data_ndx])]
```
####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
new_dailysum <- tapply(datafile_impute$steps, datafile_impute$date, sum, na.rm=TRUE, simplify=T)

hist(x=new_dailysum,
     col="red",
     breaks=20,
     xlab="daily steps",
     ylab="frequency",
     main="The distribution of daily total (with missing data imputed)")
```

![plot of chunk ImputeHist](figure/ImputeHist-1.png)
#### Mean and Median

```r
mean(new_dailysum)
```

```
## [1] 10766.19
```

```r
median(new_dailysum)
```

```
## [1] 10766.19
```
#### Q1: Do these values differ from the estimates from the first part of the assignment? 
#### Ans: Mean doesn't change, median slighly changes

#### Q2: What is the impact of imputing missing data on the estimates of the total daily number of steps?
#### Ans: Higher frequency in the histogram (center region)

## Are there differences in activity patterns between weekdays and weekends?
#### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
is_weekday <- function(d) {
    wd <- weekdays(d)
    ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(datafile_impute$date, is_weekday)
datafile_impute$wk <- as.factor(wx)
head(datafile_impute)
```

```
##       steps       date interval      wk
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```
#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
wk_datafile <- aggregate(steps ~ wk+interval, data=datafile_impute, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=wk_datafile)
```

![plot of chunk PlotIntrvl](figure/PlotIntrvl-1.png)
