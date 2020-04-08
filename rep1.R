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
mov <- aggregate(steps ~ date, data, sum)
hist(mov$steps, main = paste("Total of steps per Day"), col='Red',xlab='Number of Steps')
dev.off()

media <- mean(mov$steps)
media
mediano <- median(mov$steps)
mediano


##Working and plotting data of average activity 
## 4. Time series plot of the average number of steps taken
## 5. The 5-minute interval that, on average, contains the maximum number of steps
png('interval.png')
espaço <- aggregate(steps ~ interval, data, mean)
plot(espaço$interval,espaço$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number per Day by Interval")
dev.off()

espaçomax <- espaço[which.max(espaço$steps),1]
espaçomax

##Working and plotting data of missing values
##6. Code to describe and show a strategy for imputing missing data
## 7. Histogram of the total number of steps taken each day after missing values are imputed

Mistotal <- sum(!complete.cases(data))
Mistotal

mediapassos <- aggregate(steps ~ interval, data = data, FUN = mean)
pegaNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(mediapassos, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  pegaNA <- c(pegaNA, steps)
}

new_activity <- data
new_activity$steps <- pegaNA

png('totalpassos.png')
agregado <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(agregado$steps, main = paste("Total Steps per days"), col="green", xlab="Number of Steps")
dev.off()

mediatot <- mean(agregado$steps)
mediatot

medianotot <- median(agregado$steps)
medianotot
