Reproducible Research: Peer Assessment Assignment 1 (Week No.2)
========================================================

Name:Ho Weixing

Module: Reproducible Research 

Subject: Peer Assignment 1

# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices.  Such examples are: [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These kinds of devices are part of the "quantified self" movement: those who  measurements about themselves on a regular basis in order to improve their health,  find patterns in their behaviour, or because they are simply technology geeks. However, these data remain severely underused due to the fact that the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November 2012 and include the number of steps taken in 5 minute intervals each day.


**The objective of this assignment is to perform basic exploratory data analysis to  anonymous individual's walking patterns.  All readings were taken at particular 5-minute intervals in each days.**  


# Data Source:

**Dataset** 

Available here: [Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

**Fileformat** CSV

17,568 observations

**Variables:**

* _steps_: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* _date_: The date on which the measurement was taken in YYYY-MM-DD format

* _interval_: Identifier for the 5-minute interval in which measurement was taken


```{r setoptions, echo=TRUE}
```

## Procedure:

There are **five** steps in our analysis:

1.  **Loading in and preprocessing the data**

2.  **Plotting a histogram of the total number of steps taken each day and calculating the mean and median of each day.**


3.  **Determining the average daily activity pattern:** 
  
      Plotting a time-series plot for each 5-minute interval (the x-axis) versus the average number of steps averaged across all days (the y-axis). In other words, we are calculating the average and median of the 61 observations taken at each interval.


4.  **Imputing missing values:**

    The data has included some NA values where the amount of steps is missing value.  The presence of these missing value may introduce bias into our analysis. In this step, a simple strategy was performed to replace the missing values in the dataset with a filler value (**mean value of the 5-minute interval that was missing**).The proccessed result was compared with the generated result in the step number 3. 
    
5.  **To split up the data into weekdays and weekends and observe if there is any difference in activity patterns between these two classes**:

    Same plot is generated as what we did in the step number 3, but now two different plots are generated to reflect the activity on the weekdays and weekends.

## Loading in and preprocessing data

*Note: Please set the working directory to where your data is located.( command: **setwd()**)

Firstly, we need to read the data and process it before proceed to analysis. We unarchived the data in '.zip' archive file and read the data. Then we converted the dates into `POSIXlt` class for easier processing. 


```{r cache=TRUE}
unzip("activity.zip") # Unzip archive
dat <- read.csv("activity.csv") # Read in data file

# Turn the date data into a valid date class
# Allows for easier processing
# Dates are in YYYY-MM-DD format
dates <- strptime(dat$date, "%Y-%m-%d")
dat$date <- dates

# Keep a list of all possible days
uniqueDates <- unique(dates)
# Keep a list of all possible intervals
uniqueIntervals <- unique(dat$interval)
```

`uniqueDates` and `uniqueIntervals`are variables that store a list of all possible dates and intervals.  Therefore it was used for data processing and to  plot our necessary data.

## What is the mean total number of steps taken per day?

Based on the processed data in previous step, we need to calculate the  total number of steps taken per day and plot it into a histogram. Then it is followed by the calcuation  for the mean total number of steps taken per day.Please refer the steps belows:-

  - To split up the data into individual data frames where each data frame represents the data for a particular day.
  - To create a vector that accumulates all of the steps taken for each day and store it into another separate vector. (*Note: 'NA' value will be ignored during this analysis for the time being.)
  - To plot a histogram where the x-axis represents the particular day in question, while the y-axis denotes how many steps were taken in total for each day.
  - To calculate the mean and median number of steps per day.


```{r cache=TRUE, fig.width=11, fig.height=6}
# Part 2 - Create a histogram of the total number of steps taken
# each day
# First split up the data frame for steps by day
stepsSplit <- split(dat$steps, dates$yday)

# Next find the total number of steps over each day
totalStepsPerDay <- sapply(stepsSplit, sum, na.rm=TRUE)

# Plot a (pseudo) histogram where the x-axis denotes the day
# and the y-axis denotes the total number of steps taken 
# for each day
plot(uniqueDates, totalStepsPerDay, main="Histogram of steps taken each day", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
```

**'Mean steps per day'** result:

```{r cache=TRUE}
meanStepsPerDay <- sapply(stepsSplit, mean, na.rm=TRUE)
meanDataFrame <- data.frame(date=uniqueDates, meanStepsPerDay=meanStepsPerDay, row.names=NULL)
meanDataFrame
```

**'Median steps per day'** result:
```{r cache=TRUE}
medianStepsPerDay <- sapply(stepsSplit, median, na.rm=TRUE)
medianDataFrame <- data.frame(date=uniqueDates, medianStepsPerDay=medianStepsPerDay, row.names=NULL)
medianDataFrame
```

Some of median data are displayed as 'NA' or 0 due to the particular day has majority of the steps taken are zero and so the mean would logically represent the 50th percentile. A sample which you can refer is the data between October 10th to October 12th of 2012.


## What is the average daily activity pattern?

We need to split up this data gain so the steps taken **over each time interval** can be stored into each individual data frames. Therefore, we can calculate the means of each time intervals. (*Note: 'NA' values will be ignored for the time being).  We plot the data as a time-series plot (of `type="l"`) and locate where  the maximum is located.A red vertical line is plotted to denote this location:



```{r cache=TRUE, fig.width=11, fig.height=6}
# Part 3 - Time-series plot (type="l")
# x-axis - Time interval (5, 10, 15, ...)
# y-axis - Average number of steps taken across all days for this time interval

# Split up the data according to the interval
intervalSplit <- split(dat$steps, dat$interval)

# Find the average amount of steps per time interval - ignore NA values
averageStepsPerInterval <- sapply(intervalSplit, mean, na.rm=TRUE)

# Plot the time-series graph
plot(uniqueIntervals, averageStepsPerInterval, type="l",
     main="Average number of steps per interval across all days", 
     xlab="Interval", ylab="Average # of steps across all days", 
     lwd=2, col="blue")

# Find the location of where the maximum is
maxIntervalDays <- max(averageStepsPerInterval, na.rm=TRUE)
maxIndex <- as.numeric(which(averageStepsPerInterval == maxIntervalDays))

# Plot a vertical line where the max is
maxInterval <- uniqueIntervals[maxIndex]
abline(v=maxInterval, col="red", lwd=3)
```

With reference to the above plot, the interval that records the maximum number of steps averaged across all days is:

```{r cache=TRUE}
maxInterval
```

## Imputing missing values

Before imputing the missing value, we neeed to calculate the total number of missing values in the data, which represents the total number of observations that did not have any steps recorded (i.e. those rows which are `NA`)

```{r cache=TRUE}
# Part 4 - Calculate total amount of missing values in the data set
# Use complete.cases to find a logical vector that returns TRUE
# if it is a complete row (a.k.a. no NA values) and FALSE otherwise
completeRowsBool <- complete.cases(dat$steps)
numNA <- sum(as.numeric(!completeRowsBool))
numNA
```

The strategy which we deploy here is to fill in the missing values data set by  replacing all `NA` values with the mean of that particular 5-minute interval the observation falls on. 


```{r cache=TRUE}
# Modify the meanStepsPerDay vector that contains the mean steps taken
# for this 5 minute interval
# Each day consists of 288 intervals and there are 61 days in total
# First remove NaN values and replace with 0.  
# NaN values are produced when the entire day was filled with NA values
# Essentially the mean and median would be zero anyway!
meanStepsPerDay[is.nan(meanStepsPerDay)] <- 0

# Now create a replicated vector 288 times
# The reason why we're doing this is because the slots
# in the vector naturally line up with the interval for
# a particular day.  Now, all we have to do is find where
# in the data set there are missing steps, and simply do
# a copy from one vector to the other
meanColumn <- rep(meanStepsPerDay, 288)

# The steps before replacement
rawSteps <- dat$steps

# Find any values that are NA in the raw steps data
stepsNA <- is.na(rawSteps)

# Now replace these values with their corresponding mean
rawSteps[stepsNA] <- meanColumn[stepsNA]

# Throw these back into a new data frame
datNew <- dat
datNew$steps <- rawSteps
```

We plot a histogram based on new data again: 

```{r cache=TRUE, fig.width=11, fig.height=12}
# Repeat Part 2 now
# First split up the data frame for steps by day
stepsSplitNew <- split(datNew$steps, dates$yday)

# Next find the total number of steps over each day
# There should not be an NA values and so we don't need
# to set the flag
totalStepsPerDayNew <- sapply(stepsSplitNew, sum)

# Plot a (pseudo) histogram where the x-axis denotes the day
# and the y-axis denotes the total number of steps taken 
# for each day
par(mfcol=c(2,1))
# Plot the original histogram first
plot(uniqueDates, totalStepsPerDay, main="Histogram of steps taken each day before imputing", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
# Plot the modified histogram after
plot(uniqueDates, totalStepsPerDayNew, main="Histogram of steps taken each day after imputing", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
```

With this new data, we also re-compute the mean over all days as what we did in step number 2.  As a side-by-side comparison, we placed the data before imputing, as well as the new one in the same data frame

Note: We have replaced all of the `NaN` values to `0`.  As such, the mean steps per day of the new data are:

```{r cache=TRUE}
meanStepsPerDayNew <- sapply(stepsSplitNew, mean)
meanDataFrameNew <- data.frame(date=uniqueDates, meanStepsPerDay=meanStepsPerDay, 
                               meanStepsPerDayNew=meanStepsPerDayNew, row.names=NULL)
meanDataFrameNew
```

As same as in the above, we also did the same for the median steps per day:
```{r cache=TRUE}
medianStepsPerDayNew <- sapply(stepsSplitNew, median)
medianDataFrameNew <- data.frame(date=uniqueDates, medianStepsPerDay=medianStepsPerDay, 
                                 medianStepsPerDayNew=medianStepsPerDayNew, row.names=NULL)
medianDataFrameNew
```

By referring to the  data frames in the above, the only values that have changed are those days where all of the observations were missing (i.e. those days having all zeroes / `NA`).  The remaining of the observations have remained the same. 

With regards to the median vector, those rows that were filled with `NA` had all of their observations on that day filled with the mean value and so the median would obviously be the mean as well.

## Are there differences in activity patterns between weekdays and weekends?

With the new data sample that we processed in previous, we are going to split up the data into two data frames :

  1)One data frame consists of all steps taken on a weekday
  
  2)Another data frame consists of all steps taken on a weekend.  
  
  The following `R` code illustrates this at below:

```{r cache=TRUE}
# Part 5 - Now split up the data so that it's sorted by weekday or weekend
# We have casted the dates to a POSIXlt class so wday is part of this class
# wday is an integer ranging from 0 to 6 that represents the day of the week
# 0 is for Sunday, 1 is for Monday, going up to 6 for Saturday
# Store this into wdays
wdays <- dates$wday

# Create a new factor variable that classifies the day as either a weekday or weekend
# First, create a numeric vector with 2 levels - 1 is for a weekday, 2 for a weekend
classifywday <- rep(0, 17568) # 17568 observations overall

# Any days that are from Monday to Friday, set the numeric vector in these positions
# as 1
classifywday[wdays >= 1 & wdays <= 5] <- 1

# Any days that are on Saturday or Sunday, set the numeric vector in these positions
# as 2
classifywday[wdays == 6 | wdays == 0] <- 2

# Create a new factor variable that has labels Weekdays and Weekends
daysFactor <- factor(classifywday, levels=c(1,2), labels=c("Weekdays", "Weekends"))

# Create a new column that contains this factor for each day
datNew$typeOfDay <- daysFactor

# Now split up into two data frames
datWeekdays <- datNew[datNew$typeOfDay == "Weekdays", ]
datWeekends <- datNew[datNew$typeOfDay == "Weekends", ]
```

Once the steps in the above are done, we split up the data for each data frame so we will have two sets of individual data frames, which are represented as weekdays and and weekends in the set of individual data frames form.

Next, we calculate the mean amount of steps for each interval for the weekedays data frame and weekends data frame. It eventually gnerateds two vectors that used for weekdays and weekends respectively as per the following R codes below: 


```{r cache=TRUE, fig.width=11, fig.height=12}
# Further split up the Weekdays and Weekends into their own intervals
datSplitWeekdays <- split(datWeekdays$steps, datWeekdays$interval)
datSplitWeekends <- split(datWeekends$steps, datWeekends$interval)

# Find the average for each interval
meanStepsPerWeekdayInterval <- sapply(datSplitWeekdays, mean)
meanStepsPerWeekendInterval <- sapply(datSplitWeekends, mean)

par(mfcol=c(2,1))
plot(uniqueIntervals, meanStepsPerWeekdayInterval, type="l",
     main="Average number of steps per interval across all weekdays", 
     xlab="Interval", ylab="Average # of steps across all weekdays", 
     lwd=2, col="blue")
plot(uniqueIntervals, meanStepsPerWeekendInterval, type="l",
     main="Average number of steps per interval across all weekends", 
     xlab="Interval", ylab="Average # of steps across all weekends", 
     lwd=2, col="blue")
```


**Analysis Based on The Data Plots Across Weekday and Weekend:**

The plots shows the data has very much sum up the activtivy that a normal individual would undergo depending on whether weekday or weekend time. For both periods, the intervals between interval IDs '0' and around '525' are uniform in the trends. This result  most likely reveals these intervals when the subject was sleeping. 

The difference starts at between'525' and aboout '800'. During the weekdays, mostly likely the subject is getting ready to work or starting their days; whereras, the subjet had less movement during the weekend for these intervals.


Next difference point between both plots is at roughly the `1000` mark.  On the weekdays, this could be reflected with the subject being at work and there is not much movement.  This potentially point out the job of the  subject is required minial movment. On the other hand,  the behaviour varies wildly during the weekend for these intervals due to the subject may involve in the outdoor activities which requird extra amount of movement.


When the intervals approaching '1700'mark, both trends become similar until the intervals after '1700' mark. The subject has more movements in this interval during weekends due to the subject may be socializing.

In a nuthslle, this data and analysis report reflect the behaviour of an individual going through a normal day in each intervals during weekday and also weekend times. 

