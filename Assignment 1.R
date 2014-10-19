#read data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp, mode="wb")
unzip(temp, "activity.csv")
data <- na.omit(read.csv("activity.csv",stringsAsFactors=F))
dataNA <- read.csv("activity.csv",stringsAsFactors=F)

str(data)
head(data)

hist(aggregate(data$steps,by=list(data$date),sum)[,2],
     main="Steps per Day",
     xlab="Steps",
     col="blue")

mean(aggregate(data$steps,by=list(data$date),sum)[,2])
median(aggregate(data$steps,by=list(data$date),sum)[,2])


mean(aggregate(data$steps,by=list(data$interval),mean)[,2])

plot(unique(data$interval),
     aggregate(data$steps,by=list(data$interval),mean)[,2],
     type="l",
     main="Avg Steps per 5 Min Interval",
     xlab="5 Min Interval",
     ylab="Average Steps Per Interval")

aggregate(data$steps,by=list(data$interval),mean)[
  which.max(aggregate(data$steps,by=list(data$interval),mean)[,2]),]

dataNA[is.na(dataNA[,1]),1] <- mean(aggregate(data$steps,by=list(data$interval),mean)[,2]) 

hist(aggregate(dataNA$steps,by=list(dataNA$date),sum)[,2],
     main="Steps per Day - **NEW**",
     xlab="Steps",
     col="blue")

mean(aggregate(dataNA$steps,by=list(dataNA$date),sum)[,2])
median(aggregate(dataNA$steps,by=list(dataNA$date),sum)[,2])

data$weekday <- weekdays(as.POSIXct(data$date,"%y-%m-%d"))

data$weekdayGrp <- as.factor(
        ifelse(data$weekday == "Saturday" | data$weekday == "Sunday", 
         "Weekend", "Weekday"))

splitdata <- split(data,data$weekdayGrp)

par(mfrow=c(2,1))

plot(unique(data$interval),
     aggregate(splitdata[[1]]$steps,by=list(splitdata[[1]]$interval),mean)[,2],
     type="l",
     main="Weekday - Avg Steps per 5 Min Interval",
     ylab=""
     ylim=c(0,250))

plot(unique(data$interval),
     aggregate(splitdata[[2]]$steps,by=list(splitdata[[2]]$interval),mean)[,2],
     type="l",
     main="Weekend - Avg Steps per 5 Min Interval",
     xlab="5 Min Intervals",
     ylab="Average Steps Per Interval",
     ylim=c(0,250))

par(mfrow=c(1,1))

setwd("C:/Users/knobbe/Documents/PTO, Time Sheets, etc/Coursera/Data Science Specialization/5. Reproducable Research")
getwd()
library(knitr)
knit2html("PA1_Template.Rmd")
browseURL("PA1_Template.html")
