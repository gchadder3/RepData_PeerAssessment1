## PA1 Raw Script Code
##
## Last update: 4/8/16 (George Chadderdon)

## Do the data processing.

## IF we don't have the file yet, extract it from the activity.zip file.
if (!file.exists("activity.csv"))
{
    unzip("activity.zip")
}

## Read the data into a data.frame.
dfr <- read.csv("activity.csv")

## Load dplyr for data.frame manipulation.
library(dplyr)

## Reorder the columns so the observer indices are first and then the observed
## values follow.
dfr <- select(dfr, date, interval, steps)

## Create a table of total sums of steps for each day the user wore the device.
## (Ignore missing data in calculating these sums.)
daySteps <- group_by(dfr, date) %>% summarize(totalSteps=sum(steps, na.rm=TRUE))

## Create a table of average steps taken (averaged across all of the days)
## for each 5 minute interval during the day.
intervalAveSteps <- group_by(dfr, interval) %>% 
    summarize(aveSteps=mean(steps, na.rm=TRUE))


## Analyze the data.

## Plot a histogram of the number of total steps taken during the days.
hist(daySteps$totalSteps, main="Histogram of step counts for the 61 days", 
     xlab="Steps Taken During the Day", ylab="Frequency (days)")

## Calcuate the mean and median of the total steps taken during the days.
totStepsMean <- mean(daySteps$totalSteps)
totStepsMed <- median(daySteps$totalSteps)

## Plot the average steps taken (averaged over days) vs. the 5 minute interval.
plot(intervalAveSteps$interval, intervalAveSteps$aveSteps, type="l", 
     main="Average Activity vs. 5-Minute Interval", 
     xlab="5 Minute Interval Index", ylab="Average Activity (Steps)")

## Find the interval where the average steps taken is maximum.
rowInd <- which(intervalAveSteps$aveSteps==max(intervalAveSteps$aveSteps))
maxStepInterval <- intervalAveSteps$interval[rowInd]


## Perform imputation of NA data.  For the method, use the means calculated for
## each interval (across days) to fill in the NAs for the particular intervals.
## The means will be rounded to the nearest integer.

## Remember the number of NAs in the data.
numNAs <- sum(is.na(dfr$steps))

## Start by creating a merge table which copies over the average step values 
## for each interval.
dfr.imp <- merge(dfr, intervalAveSteps, by.x="interval", by.y="interval")

## Round the aveSteps values to the nearest integer.
dfr.imp <- mutate(dfr.imp, aveSteps=round(aveSteps))

## For all of the NA step values, copy over the rounded aveSteps values.
dfr.imp[is.na(dfr.imp$steps), ]$steps <- 
    dfr.imp[is.na(dfr.imp$steps), ]$aveSteps

## Sort by date and then interval, and then reorder the columns and remove
## the extra one that was created before.
dfr.imp <- arrange(dfr.imp, date, interval) %>%
    select(date, interval, steps)


## Do remaining analyses with the imputed data.

## Create a table of total sums of steps for each day the user wore the device.
## This uses the imputed NA data.
daySteps.imp <- group_by(dfr.imp, date) %>% summarize(totalSteps=sum(steps))
                                              
## Plot a histogram of the number of total steps taken during the days.
hist(daySteps.imp$totalSteps, 
     main="Histogram of step counts for the 61 days (imputed data)", 
     xlab="Steps Taken During the Day", ylab="Frequency (days)")

## Calcuate the mean and median of the total steps taken during the days.
totStepsMean.imp <- mean(daySteps.imp$totalSteps)
totStepsMed.imp <- median(daySteps.imp$totalSteps)

## Build a vector of string values for which weekday we are at for each date.
isWeekend <- weekdays(as.Date(as.character(dfr.imp$date))) %in% 
    c("Saturday", "Sunday")

## Add a column "weektime" that specifies whether we are on a weekday or
## weekend.
dfr.imp <- mutate(dfr.imp, weektime=factor(sapply(isWeekend, 
    function(x) {if (x) "weekend" else "weekday"})))

## Calculate average (over days) steps taken pooled by weekend and weekday
## days.
intervalWTAveSteps <- group_by(dfr.imp, interval, weektime) %>%
    summarize(aveSteps=mean(steps))

## Load ggplot2.
library(ggplot2)

## Plot panel plot showing the average activity vs. 5-minute interval for
## weekdays and weekends.
qplot(interval, aveSteps, data=intervalWTAveSteps, facets=. ~ weektime, 
      geom=c("point", "line")) + 
    labs(title="Average Activity (Weekday and Weekend) vs. 5-Minute Interval") +
    labs(x="5 Minute Interval Index") + labs(y="Average Activity (Steps)")


