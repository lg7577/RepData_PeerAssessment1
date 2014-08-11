#data load
ActData <- read.csv("activity.csv", header = TRUE, sep = ",")

#data exploration and cleaning
str(ActData)

#Question 1: What is mean total number of steps taken per day?
aggdata <-aggregate(ActData$steps, by= list(ActData$date), 
                    FUN=sum, na.rm=FALSE)
names(aggdata) <- c("date", "sum_steps")
hist(as.numeric(aggdata$sum_steps) ,col = "Red", 
     main = "Step aggregation (group) by Date", 
     xlab = "Step groups", ylab = "# of days per group", breaks = 10, 
     ylim = range(1:10))

aggdata_sum <- aggregate(steps ~ date, data = ActData, sum)
aggdata_mean <- mean(aggdata_sum$steps, na.rm = FALSE)
aggdata_median <- median(aggdata_sum$steps, na.rm = FALSE)

#Question 2: What is the average daily activity pattern?

aggdata_interval <- aggregate(steps ~ interval, data = ActData, mean)
with(aggdata_interval,plot(interval,steps,
                     main = "Average 5-minute interval across all days",       
                     xlab = "5 Minute interval",ylab = "Average Steps", 
                     type = "l"))
max_interval <- max(aggdata_interval$steps, na.rm = FALSE)
aggdata_interval[aggdata_interval$steps == max_interval,]

#Question 3: Imputing missing values

sum(is.na(ActData$steps))

ActData_nona <- ActData
index <- is.na(ActData$steps)
ActData_nona$steps <- replace(ActData$steps, index == TRUE, 0.001)

aggdata_nona <-aggregate(ActData_nona$steps, by= list(ActData_nona$date), 
                    FUN=sum, na.rm=FALSE)
names(aggdata_nona) <- c("date", "sum_steps")
hist(as.numeric(aggdata_nona$sum_steps) ,col = "Red", 
     main = "Step aggregation (group) by Date", 
     xlab = "Step groups", ylab = "# of days per group", breaks = 10, 
     ylim = range(1:10))

aggdata_sum_nona <- aggregate(steps ~ date, data = ActData_nona, sum)
aggdata_mean_nona <- mean(aggdata_sum_nona$steps, na.rm = FALSE)
aggdata_median_nona <- median(aggdata_sum_nona$steps, na.rm = FALSE)

#Question 4: Are there differences in activity patterns between weekdays 
#            and weekends?

ActData_nona$day <- weekdays(as.Date(ActData_nona$date, "%Y-%m-%d"), 
                                     abbreviate = FALSE)
ActData_nona$weekendInd <- (ActData_nona$day %in% c('Saturday','Sunday'))
ActData_nona$weekendInd <- replace(ActData_nona$weekendInd, 
                                   ActData_nona$weekendInd == TRUE, 
                                   "weekend")
ActData_nona$weekendInd <- replace(ActData_nona$weekendInd, 
                                   ActData_nona$weekendInd == FALSE, 
                                   "weekday")

ActData_interval_weekend <- aggregate(steps ~ interval + weekendInd,
                                      data = ActData_nona, sum)

g <- ggplot(ActData_interval_weekend, 
            aes(ActData_interval_weekend$interval, 
                ActData_interval_weekend$steps))
g + geom_point() + 
  facet_wrap(~ weekendInd, nrow = 2, ncol = 1) + 
  geom_line() +
  geom_point( size=2, shape=21, fill="white") +
  ggtitle("Activity patterns between weekdays and weekends") +
  xlab("Interval") +
  ylab("# of Steps")

  

