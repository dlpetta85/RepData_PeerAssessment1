library("dplyr")
library("lattice")

##Getting the source
##1. Code for reading in the dataset and/or processing the data
if (!file.exists("activity.csv") )
{
  dlurl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
  download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
  unzip('repdata%2Fdata%2Factivity.zip')
}
data <- read.csv("activity.csv")  


##Working and plotting data of steps per day
##2. Histogram of the total number of steps taken each day
##3. Mean and median number of steps taken each day
png('steps.png')

mov <- data %>% group_by(date) %>% summarise(steps = sum(steps))
hist(mov$steps, col="light blue",xlab = "Total number of steps by day", main = "Number of steps by day")
mean(mov$steps,na.rm = TRUE)

dev.off()

##Working and plotting data of average activity 
## 4. Time series plot of the average number of steps taken
## 5. The 5-minute interval that, on average, contains the maximum number of steps
png('interval.png')

median(mov$steps,na.rm = TRUE)
espaço <- data %>% group_by(interval) %>% summarise(steps = mean(steps,na.rm=TRUE))
plot(espaço$interval,espaço$steps,type="l",xlab="5 minutes interval", ylab="Average nº steps",main="Average daily pattern")
espaço$interval[which.max(espaço$steps)]

dev.off()

##Working and plotting data of missing values
##6. Code to describe and show a strategy for imputing missing data
##7. Histogram of the total number of steps taken each day after missing values are imputed
png('totalpassos.png')
sum(is.na(data$steps))

datatrab <- data
names(espaço)[2] <- "avg_steps"
datamesc <- merge(datatrab,espaço)
datamesc[which(is.na(datamesc$steps)),]$steps <- datamesc[which(is.na(datamesc$steps)),]$avg_steps
datafim <- datamesc[1:3]
mov2 <- datafim %>% group_by(date) %>% summarise(steps = sum(steps))

hist(mov2$steps, col="light blue",xlab = "Number of steps", 
     main = "Total steps per day")

dev.off()

mean(mov2$steps,na.rm = TRUE)
median(mov2$steps,na.rm = TRUE)


##Working and plotting data to split pattern
##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
##9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
png('Comparison.png')

datafim <- datafim %>% mutate(day_type = ifelse(grepl("S(at|un)",weekdays(as.Date(datafim$date, 
                                                                                        format = "%Y-%m-%d"))),"weekend","weekday"))
datafim$day_type <- as.factor(datafim$day_type)

espaço2 <- datafim %>% group_by(interval,day_type) %>% summarise(steps = mean(steps,na.rm=TRUE))
xyplot(steps ~ interval | day_type, data = espaço2, type="l",xlab="Interval",ylab="Number of steps",main="Comparison between weekday and weekend activities",
       col.line = "red",
       layout=c(1,2))

dev.off()

