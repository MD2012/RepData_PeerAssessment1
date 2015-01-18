# Reproducible Research: Peer Assessment 1

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## Loading and preprocessing the data
1. Unzip raw data file

```r
unzip("activity.zip")
```
2. Read extracted csv file

```r
data_raw <- read.csv("activity.csv", sep=",", header=TRUE, na.strings="NA")
```
3. Preprocess

```r
data_raw$date <- as.character(data_raw$date)
data_raw$interval <- as.numeric(as.character(data_raw$interval))
data_filtered <- get_filtered(data_raw)
```

## What is mean total number of steps taken per day?

```r
total_steps <- get_total_steps(data_filtered)
gen_hist(total_steps)
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
mean_o <- gen_mean(total_steps)
median_o <- gen_median(total_steps)
median_o
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
ts_interval <- aggregate(data_filtered$steps, list(date=data_filtered$interval), mean)
plot(ts_interval, type='l')
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
max_steps_interval <- ts_interval[which.max(ts_interval$x),]$date
```



## Imputing missing values

```r
#4.1
total_nas <- sum(is.na(data_raw$steps))
#4.2
na_idcs <- is.na(data_raw$steps)
na_rows <- data_raw[na_idcs,]
for(i in 1:length(na_rows$steps)) {
na_rows[i,]$steps <- compute_missing_value()
}
#4.3
data_new <- data_raw
data_new[na_idcs,] <- na_rows
#4.4 
ts <- get_total_steps(data_new)
gen_hist(ts)
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
mean_comp <- gen_mean(ts)
median_comp <- gen_median(ts)
# Do these values differ from the estimates from the first part of the assignment? 
mean_diff <- mean_o - mean_comp;
median_diff <- median_o - median_comp;
# Yes may differ, as computed values are not NA, hence may change computational statistics like mean/median too
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Depends on imputation algorithm
```



## Are there differences in activity patterns between weekdays and weekends?

```r
#5.1
wend_idcs <- (weekdays(as.Date(data_new$date))=='Samstag' | weekdays(as.Date(data_new$date))=='Sonntag')
fv <- factor(wend_idcs, labels=c('WEEKDAY','WEEKEND'))
data_new$fv <- fv
#5.2
ts_interval_new <- aggregate(data_new$steps, list(interval=data_new$interval, fv=data_new$fv), mean)
colnames(ts_interval_new)[3]<-'num_of_steps'
xyplot(num_of_steps ~ interval | fv, data=ts_interval_new, layout=c(1,2), type='l')
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png) 
