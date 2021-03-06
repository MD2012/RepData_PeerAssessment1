# Reproducible Research: Peer Assessment 1
## Appendix: Helper Functions

```r
init <- function() {
  rm(list=ls())
  Sys.setlocale("LC_TIME","en_US")  
  library(dplyr)
  library(lattice) 
}
get_filtered <- function(dat) {
  d <- filter(dat, !is.na(dat$steps))
}
get_total_steps <- function(dat) {
  total_steps <- aggregate(dat$steps, list(date=dat$date), sum)
}
gen_hist <- function(dat, clabel) {
  hist(dat$x,xlab='Daily Total Steps', main=clabel)  
}
gen_mean <- function(dat) {
  mean_ts <- mean(dat$x)
}
gen_median <- function(dat) {
  median_ts <- median(dat$x)
}
compute_missing_value <- function() {
  r <- 1
}
```

## Loading and preprocessing the data
1 Init Workspace Settings

```r
init()
```

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
2 Unzip raw data file

```r
unzip("activity.zip")
```
3 Read extracted csv file

```r
data_raw <- read.csv("activity.csv", sep=",", header=TRUE, na.strings="NA")
```
4 Preprocess

```r
data_raw$date <- as.character(data_raw$date)
data_raw$interval <- as.numeric(as.character(data_raw$interval))
data_filtered <- get_filtered(data_raw)
```

## What is mean total number of steps taken per day?

```r
total_steps <- get_total_steps(data_filtered)
gen_hist(total_steps,'Total Number of steps taken each day')
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
mean_o <- round(gen_mean(total_steps), digits=2)
median_o <- round(gen_median(total_steps), digits=2)
```
The mean number of steps taken each day is reported as: **10766.19**.  
The median number of steps taken each day is reported as: **10765**.


## What is the average daily activity pattern?

```r
ts_interval <- aggregate(data_filtered$steps, list(date=data_filtered$interval), mean)
plot(ts_interval, type='l', ylab='average number of steps taken (averaged across all days)', xlab='5 minute intervals')
```

![](./PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
max_steps_interval <- ts_interval[which.max(ts_interval$x),]$date
```
The 5-minute interval that, on average, contains the maximum number of steps is the **835** interval.


## Imputing missing values
A strategy for imputing missing values:  
1.  take full raw data set, check if it contains NA values  
2.  find the indices of the NA rows within  
3.  extract NA-rows into seperate data.frame  
4.  loop over all entries of new data.frame, compute and assign new values  
5.  merge back the enriched dataset of 5. into 1.  


```r
#ad 1
total_nas <- sum(is.na(data_raw$steps))
#ad 2
na_idcs <- is.na(data_raw$steps)
#ad 3
na_rows <- data_raw[na_idcs,]
#ad 4
for(i in 1:length(na_rows$steps)) {
  na_rows[i,]$steps <- compute_missing_value()
}
#ad 5
data_new <- data_raw
data_new[na_idcs,] <- na_rows
```
Hence, total number of steps taken each day after missing data imputation is as follows:

```r
ts <- get_total_steps(data_new)
gen_hist(ts, 'Total Number of steps taken each day (after missing data imputation)')
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Mean and median value re-computation:

```r
mean_comp <- gen_mean(ts)
median_comp <- gen_median(ts)
```
Do these values differ from the estimates from the first part of the assignment? 

```r
mean_diff <- mean_o - mean_comp;
median_diff <- median_o - median_comp;
```
Yes, for the given simple imputation algorithm:  
The difference in mean values is reported as: **1374.19**  
and the difference in median values is reported as: **370**.

What is the impact of imputing missing data on the estimates of the total daily number of steps?  
Depends on imputation algorithm.



## Are there differences in activity patterns between weekdays and weekends?

```r
wend_idcs <- (weekdays(as.Date(data_new$date))=='Saturday' | weekdays(as.Date(data_new$date))=='Sunday')
fv <- factor(wend_idcs, labels=c('WEEKDAY','WEEKEND'))
data_new$fv <- fv
ts_interval_new <- aggregate(data_new$steps, list(interval=data_new$interval, fv=data_new$fv), mean)
colnames(ts_interval_new)[3]<-'num_of_steps'
xyplot(num_of_steps ~ interval | fv, data=ts_interval_new, layout=c(1,2), type='l')
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
