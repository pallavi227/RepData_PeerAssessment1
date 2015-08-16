rm(list=ls())

library(dplyr)
library(ggplot2)

setwd ("M:\\Pallavi\\DataScience\\RepResearch\\Week2\\Assn\\RepData_PeerAssessment1\\")
data <- read.csv("activity\\activity.csv")

dataDT <- group_by(data, date)
dataDT <-  summarise(dataDT, total=sum(steps, na.rm = TRUE))

mean <- mean(dataDT$total)
median <- median(dataDT$total)


hist(dataDT$total, breaks = 20, main="Number of Steps", 
     xlab="Total number of steps taken each day", ylab = "Number of Days", col="red")
abline(v=mean, col="yellow", lwd=3)
abline(v=median, col="blue", lwd=3)

legend(x="topright", legend=c("mean","median"), col=c("yellow","blue"), bty="n", lwd=3)

