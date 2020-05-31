---
output: 
  html_document: 
    keep_md: yes
---
# **Reproducible Research Project 1**


### 1-Code for reading in the dataset and/or processing the data


```r
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))

activity <- read.csv("repdata_data_activity/activity.csv")
str(activity); summary(activity); colSums(is.na(activity))
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```
##    steps     date interval 
##     2304        0        0
```

### 2-Histogram of the total number of steps taken each day


```r
steps_per_day <- activity %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps ,na.rm = T))
```


```r
ggplot(steps_per_day ,aes(total_steps))+
        geom_histogram(bins = 30 ,fill="purple" ,alpha=0.4)+
        ggtitle("Total steps per day")+ xlab("Toal steps")+
        theme_minimal()
```

![](PA1_templete_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## 3-Mean and median number of steps taken each day


```r
mean_day_steps <-mean(steps_per_day$total_steps ,na.rm = TRUE)
median_day_steps <- median(steps_per_day$total_steps ,na.rm = TRUE)

mean_day_steps ; median_day_steps
```

```
## [1] 9354.23
```

```
## [1] 10395
```

## 4-Time series plot of the average number of steps taken


```r
average_steps <- activity %>%
        group_by(interval)%>%
        summarise(mean_stps =mean(steps ,na.rm = TRUE))

ggplot(average_steps)+
        geom_line(aes(x=interval ,y=mean_stps) ,color="purple")+
        ggtitle("Time Series Plot of Average Steps")+
        ylab("Average Steps") + xlab("Intervals")+theme_minimal() 
```

![](PA1_templete_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## 5-The 5-minute interval that, on average, contains the maximum number of steps


```r
average_steps[which.max(average_steps$mean_stps),]
```

```
## # A tibble: 1 x 2
##   interval mean_stps
##      <int>     <dbl>
## 1      835      206.
```

## 6- Code to describe and show a strategy for imputing missing data


```r
mean_new_steps <- mean(activity$steps ,na.rm = TRUE)
steps_without_missing <- ifelse(is.na(activity$steps), mean_new_steps ,activity$steps)
sum(is.na(steps_without_missing))
```

```
## [1] 0
```

```r
new.data <- cbind(activity[,c("date" ,"interval")],steps_without_missing)
head(new.data)
```

```
##         date interval steps_without_missing
## 1 2012-10-01        0               37.3826
## 2 2012-10-01        5               37.3826
## 3 2012-10-01       10               37.3826
## 4 2012-10-01       15               37.3826
## 5 2012-10-01       20               37.3826
## 6 2012-10-01       25               37.3826
```


## 7-Histogram of the total number of steps taken each day after missing values are imputed


```r
complete_steps_per_day <- new.data %>%
        group_by(date) %>%
        summarise(stps = sum(steps_without_missing ))

ggplot(complete_steps_per_day)+
        geom_histogram(aes(x=stps),binwidth = 1000 ,fill="purple" ,alpha=0.5)+
        ggtitle("Total number of steps per day after missing values are imputed")+
        xlab("Steps")+theme_minimal()
```

![](PA1_templete_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## 8-Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
is_weekday <-function(date){
        ifelse(wday(date)%in%c(1,7),"weekend","weekday")
        
}
new.data <- mutate(new.data,date=ymd(date)) %>% mutate(day=sapply(date,is_weekday))
head(new.data)
```

```
##         date interval steps_without_missing     day
## 1 2012-10-01        0               37.3826 weekday
## 2 2012-10-01        5               37.3826 weekday
## 3 2012-10-01       10               37.3826 weekday
## 4 2012-10-01       15               37.3826 weekday
## 5 2012-10-01       20               37.3826 weekday
## 6 2012-10-01       25               37.3826 weekday
```


```r
averages <- new.data %>%
        group_by(interval ,day) %>%
        summarise(avr_steps = mean(steps_without_missing))

ggplot(averages ,aes(interval , avr_steps))+
        geom_line(color="purple")+
        facet_wrap(day~.)+
        xlab("Intervals")+ ylab("Steps")+
        ggtitle("Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends")+
        theme_minimal()
```

![](PA1_templete_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
