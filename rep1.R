##Getting the source
if (!file.exists("activity.csv") )
{
  dlurl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
  download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
  unzip('repdata%2Fdata%2Factivity.zip')
}
data <- read.csv("activity.csv")  

##Working and plotting data
png('steps.png')
mov <- aggregate(steps ~ date, data, sum)
hist(mov$steps, main = paste("Total of steps per Day"), col='Red',xlab='Number of Steps')
dev.off()

med <- mean(mov$steps)
med