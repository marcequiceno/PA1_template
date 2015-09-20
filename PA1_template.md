---
title: "Reproducible Research/Peer Assessments 1"
output: html_document
---


Loading data

```r
df <- read.csv("activity.csv", sep = ",")
```

Total number of steps taken per day

```r
df_day <- aggregate(df$steps,by=list((substr(df$date,1,17568))),sum)
names(df_day) <- c("date", "steps")
```

Histogram of the total number of steps taken each day

```r
hist(df_day$steps, main = "Total number of steps taken each day (with NA)", xlab = "Total steps by day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Mean and median of the total number of steps taken per day

```r
Mean <- mean(df_day$steps, na.rm = TRUE)
Median <- median(df_day$steps, na.rm = TRUE)
paste("Mean of the total number of steps taken per day = ", Mean)
```

```
## [1] "Mean of the total number of steps taken per day =  10766.1886792453"
```

```r
paste("Median of the total number of steps taken per day = ", Median)
```

```
## [1] "Median of the total number of steps taken per day =  10765"
```

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
df_int <- aggregate(steps ~ interval, data = df, mean, na.rm = TRUE)
plot(df_int, xlab= "Interval", ylab = "Steps by interval (mean)")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

5-minute interval that contains the maximum number of steps

```r
Max_int <- df_int[which.max(df_int[,2]),1]
paste("5-minute interval that, on average, contains the maximum number of steps =", Max_int)
```

```
## [1] "5-minute interval that, on average, contains the maximum number of steps = 835"
```

Total number of missing values in the dataset

```r
na <- sapply(df, function(x) sum(is.na(x)))
print(na)
```

```
##    steps     date interval 
##     2304        0        0
```

Filling in all of the missing values in the dataset with the mean for that 5-minute interval

```r
int_steps <- function(interval) {
  df_int[df_int$interval == interval, ]$steps
}

df_fill <- df
count = 0
for (i in 1:nrow(df_fill)) {
  if (is.na(df_fill[i, ]$steps)) {
    df_fill[i, ]$steps <- int_steps(df_fill[i, ]$interval)
    count = count + 1
  }
}
paste("strategy for imputing missing data: fill all of the missing values with the mean for that 5-minute interval")
```

```
## [1] "strategy for imputing missing data: fill all of the missing values with the mean for that 5-minute interval"
```

Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day

```r
df_day_fill <- aggregate(steps ~ date, data = df_fill, sum)
hist(df_day_fill$steps, main = "Total number of steps taken each day (without NA)", xlab = "Total steps by day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
Mean_fill <- mean(df_day_fill$steps)
Median_fill <- median(df_day_fill$steps)
print(Mean)
```

```
## [1] 10766.19
```

```r
print(Median)
```

```
## [1] 10765
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
Dif_Mean <- Mean - Mean_fill
Dif_Median <- Median - Median_fill
print(Dif_Mean)
```

```
## [1] 0
```

```r
print(Dif_Median)
```

```
## [1] -1.188679
```

Differences in activity patterns between weekdays and weekend

```r
df_fill$day_type = ifelse(as.POSIXlt(as.Date(df_fill$date))$wday%%6 == 0, "weekend", "weekday")
df_fill$day_type = factor(df_fill$day_type, levels = c("weekday", "weekend"))
```

Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
df_day_fill_w = aggregate(steps ~ interval + day_type, df_fill, mean)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.2
```

```r
xyplot(steps ~ interval | factor(day_type), data = df_day_fill_w, aspect = 1/2, type = "l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
