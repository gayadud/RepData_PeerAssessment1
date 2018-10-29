#download the data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "data.zip")
unzip("data.zip")

#load data in R
Data <- read.csv("activity.csv")

#look at data
summary(Data)
str(Data)

#convert date to "date" format
Data$date <- as.Date(Data$date, "%Y-%m-%d")

#compute total steps taken per day
Steps_sum <- aggregate(steps ~ date, data= Data, FUN = sum)

#plot the sum
plot(Steps_sum[,2], type="h", lwd=10, main="Total steps taken per day",
     xlab="Date", ylab="Steps", xaxt = "n")
axis(side=1, at = c(1,15,30,45,60), labels=c("Oct-01", "Oct-15", "Nov-01", "Nov-15", "Dec-01"))

#calculating mean and median of steps per day
Steps_mean <- aggregate(steps ~ date, data= Data, FUN = mean)
Steps_median <- aggregate(steps ~ date, data= Data, FUN = median)

#calculate average of steps at 5 minute intervals across all days
Steps_average_all_days <- aggregate(steps ~ interval, data= Data, FUN = mean)

#plot the time series
plot(Steps_average_all_days[,2], type = "l", main = "Average steps at 5 min intervals across all days",
     xlab = "interval", ylab = "steps")

#computing 5 min interval with maximum number of steps
max(Steps_average_all_days, na.rm=TRUE)

#calculating total number of misssing rows in data
sum(is.na(Data))

#replacing missing steps with total mean of steps for all the days
mean(Data$steps, na.rm=TRUE)
Data$steps[is.na(Data$steps)] <- mean(Data$steps, na.rm=TRUE)
Imputed_data <- transform(Data, steps = ifelse(is.na(Data$steps), 
                                               mean(Data$steps, na.rm=TRUE),
                                               Data$steps))

#computing total number of steps taken, mean and median per day for imputed data
Steps_sum_imputed <- aggregate(steps ~ date, data=Imputed_data, FUN = sum)
Steps_mean_imputed <- aggregate(steps ~ date, data=Imputed_data, FUN = mean)
Steps_median_imputed <- aggregate(steps ~ date, data=Imputed_data, FUN = median)

#plot histogram of total number of days from imputed data and compare with original data
par(mfrow = c(1,2))

plot(Steps_sum[,2], type="h", lwd=5, main="Original Data(With NA)",
     xlab="Date", ylab="Steps", xaxt = "n")
axis(side=1, at = c(1,15,30,45,60), labels=c("Oct-01", "Oct-15", "Nov-01", "Nov-15", "Dec-01"))

plot(Steps_sum_imputed[,2], type="h", lwd=5, main="Imputed Data(without NA)",
     xlab="Date", ylab="Steps", xaxt = "n")
axis(side=1, at = c(1,15,30,45,60), labels=c("Oct-01", "Oct-15", "Nov-01", "Nov-15", "Dec-01"))

#load dplyr library
library(dplyr)

#adding factor column with weekday and weekend values
Imputed_data <- mutate(Imputed_data, day = weekdays(Imputed_data$date))
#changing column to factor
Imputed_data$day <- as.factor(Imputed_data$day)

#changing levels of the column to weekday and weekend
levels(Imputed_data$day) <- list(weekday = c("Monday", "Tuesday",
                                              "Wednesday", "Thursday",
                                              "Friday"), weekend =
                                    c("Saturday", "Sunday"))

#compute average number of steps taken across weekday and weekend days
Weekday_mean <- aggregate(steps ~ interval, data=Imputed_data[Imputed_data$day == "weekday",], FUN = mean)
Weekend_mean <- aggregate(steps ~ interval, data=Imputed_data[Imputed_data$day == "weekend",], FUN = mean)

#plot the weekday and weekend average steps
par(mfrow=c(2,1))

plot(Weekday_mean$interval,Weekday_mean$steps, type = "l", main="Weekdays", xlab="Interval", ylab="Steps")
plot(Weekend_mean$interval,Weekend_mean$steps, type = "l", main="Weekend", xlab="Interval", ylab="Steps")
