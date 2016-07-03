---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(dplyr)
```

## Loading and preprocessing the data
The code bellow expects activity.zip to be extracted.


```r
data = read.csv('activity.csv')
data$date = as.Date(data$date)
head(data, n=5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

Five first rows does not tell us much about data set. Let's see random sample:


```r
data[sample(nrow(data), 5),]
```

```
##       steps       date interval
## 12456     0 2012-11-13      555
## 8113      0 2012-10-29      400
## 9508      0 2012-11-03       15
## 8851    148 2012-10-31     1730
## 17414    NA 2012-11-30     1105
```

## What is mean total number of steps taken per day?


```r
total_steps = sapply(split(data$steps,data$date), sum, na.rm=TRUE)
```

The mean and median of the total number of steps taken per day:


```r
mean(total_steps)
```

```
## [1] 9354.23
```

```r
median(total_steps)
```

```
## [1] 10395
```

Total steps distribution:


```r
hist(total_steps, breaks=10)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## What is the average daily activity pattern?


```r
timeline = aggregate(steps ~ interval, data, mean)
plot(timeline, type='l', xlab="Interval", ylab="Steps")
title(main = 'Steps Taken over a Day')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


```r
timeline = aggregate(steps ~ interval, data, mean)
print(timeline$interval[max(timeline$steps) == timeline$steps])
```

```
## [1] 835
```

## Imputing missing values


```r
# Incomplete readings:
sum(!complete.cases(data))
```

```
## [1] 2304
```

```r
# Complete readings:
sum(complete.cases(data))
```

```
## [1] 15264
```

Let's impute the missing data with the mean for that 5-minute interval: 


```r
avg_steps <- sapply(split(data$steps, data$interval), mean, na.rm=TRUE)
imputed <- ifelse(is.na(data$steps), avg_steps[as.character(data$interval)], data$steps)

imputed_data = mutate(data, steps = imputed)
```


```r
total_steps_imputed = sapply(split(imputed_data$steps,imputed_data$date), sum, na.rm=TRUE)
```

Total steps distribution:


```r
hist(total_steps_imputed, breaks=10)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

The mean and median of the total number of steps taken per day:


```r
mean(total_steps_imputed)
```

```
## [1] 10766.19
```

```r
median(total_steps_imputed)
```

```
## [1] 10766.19
```

**Both mean and median are higher. Inputing values increases the estimates of the total daily number of steps.**

## Are there differences in activity patterns between weekdays and weekends?

There is a difference. It seems that people walk more over the weekend.


```r
data$dow <- as.factor(ifelse(as.POSIXlt(data$date)$wday %in% c(0,6), 'weekend', 'weekday'))

weekdays = data[data$dow == 'weekday',]
weekends = data[data$dow == 'weekend',]

weekdays_timeline = aggregate(steps ~ interval, weekdays, mean)
weekends_timeline = aggregate(steps ~ interval, weekends, mean)

plot(weekends_timeline$interval, weekends_timeline$steps, type='l', xlab = 'Interval', ylab = 'Steps', col='black')
lines(weekdays_timeline$interval, weekdays_timeline$steps, type='l', col='red')
legend("topleft", c("weekends", "weekdays"), col = c('black', 'red'), lty = c(1, 1))
title(main = 'Steps Taken over a Day')
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
