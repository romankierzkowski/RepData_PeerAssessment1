# Reproducible Research: Peer Assessment 1

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
## 15748   752 2012-11-24     1615
## 8715      0 2012-10-31      610
## 5260      0 2012-10-19      615
## 8791    170 2012-10-31     1230
## 9940     NA 2012-11-04     1215
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

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

## What is the average daily activity pattern?


```r
timeline = aggregate(steps ~ interval, data, mean)
plot(timeline, type='l', xlab="Interval", ylab="Steps")
title(main = 'Steps Taken over a Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)


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

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)

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

library(lattice) 
aggregated = aggregate(steps ~ interval * dow, data, mean)
xyplot(steps ~ interval | dow, main="", ylab="Steps", xlab="Interval", data=aggregated, type="l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)

