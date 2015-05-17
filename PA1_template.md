---
title: "Reproducible Research: Peer Assessment 1"
output: "PA1_template.md"
  html_document: "PA1_template.html"
    keep_md: true
---

### Load required packages

```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data

```r
if(!file.exists("activity.csv"))
  unzip("activity.zip")

rawData <- read.csv("activity.csv") %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))
str(rawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

```r
actPerDay <- rawData %>%
  group_by(date) %>%
  summarise(steps = sum(steps, na.rm = TRUE))

ggplot(actPerDay, aes(steps/1e3)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(x = "Steps (thousands)",
       y = "Day Occurence")
```

![plot of chunk histogram-steps](figure/histogram-steps-1.png) 

```r
summarise(actPerDay, mean(steps), median(steps))
```

```
## Source: local data frame [1 x 2]
## 
##   mean(steps) median(steps)
## 1     9354.23         10395
```

## What is the average daily activity pattern?

```r
actPerInt <- rawData %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))

ggplot(actPerInt, aes(interval, steps)) +
  geom_line() + 
  theme_bw() +
  labs(x = "Interval",
       y = "Number of Average Steps")
```

![plot of chunk average-daily-pattern](figure/average-daily-pattern-1.png) 

```r
actPerInt$interval[ which.max(actPerInt$steps) ]
```

```
## [1] 835
```

## Imputing missing values

```r
sum(is.na(rawData$steps))
```

```
## [1] 2304
```

Impute these missing values using average steps by interval:


```r
imputedDF <- rawData %>%
  left_join(actPerInt, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps.x), steps.y, steps.x))
```


```r
imputedActByDay <- imputedDF %>%
  group_by(date) %>%
  summarise(steps = sum(steps))

ggplot(imputedActByDay, aes(steps/1e3)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(x = "Steps (thousands)",
       y = "Day Occurence")
```

![plot of chunk imputed-hist](figure/imputed-hist-1.png) 

```r
summarise(imputedActByDay, mean(steps), median(steps))
```

```
## Source: local data frame [1 x 2]
## 
##   mean(steps) median(steps)
## 1    10766.19      10766.19
```

The histogram is very similar to the one for the first question, execept that there are far less days with zero step. This makes sense because `NA`s are imputed with interval average. On top of that, mean value increases while median is roughly the same.

## Are there differences in activity patterns between weekdays and weekends?

```r
partitionedDF <- imputedDF %>%
  mutate(weekday = as.POSIXlt(date)$wday) %>%
  mutate(weekday = weekday > 0 & weekday < 6) %>%
  mutate(weekday = factor(weekday, labels = c("Weekend", "Weekday"))) %>%
  group_by(weekday, interval) %>%
  summarise(steps = mean(steps))

ggplot(partitionedDF, aes(interval, steps)) +
  geom_line() +
  facet_grid(weekday ~ .) +
  theme_bw() +
  labs(x = "Interval",
       y = "Number of Average Steps")
```

![plot of chunk weekday-vs-weekend](figure/weekday-vs-weekend-1.png) 
