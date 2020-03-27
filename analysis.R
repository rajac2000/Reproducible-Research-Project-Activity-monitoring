
# Get Data ----


activityOriginal <- read.csv("activity.csv")

activityOriginal$date <- as.Date(activityOriginal[,2])

NAs   <- is.na(activityOriginal$steps)
Zeros <- activityOriginal$steps == 0


# Ignore rows with NA from the table ----

#activityData <- activityOriginal[!NAs & !Zeros,]

#activityData$steps[NAs]

activityData <- activityOriginal[!NAs,]

#activityData$steps[NAs] = 0


#activityData$date <- as.Date(activityData[,2])

# sum ----
# Calculate the total number of steps taken per day

activityByDate <- setNames(aggregate(activityData$steps, 
                                     list(activityData$date), 
                                     sum),c("date","TotalSteps"))

# meadian and mean  ----
# Calculate and report the mean and median 
# of the total number of steps taken per day

summary(activityByDate$TotalSteps)[3]

summary(activityByDate$TotalSteps)[4]



# meanByDate <- setNames(aggregate(activityData$steps, 
#                                  list(activityData$date), 
#                                  mean),c("date","Mean"))
# 
# medianByDate <- setNames(aggregate(activityData$steps,
#                                    list(activityData$date),
#                                    median),c("date","Median"))

#medianByDate2 <- lapply(activityData$steps, median)

# activityByDate <- cbind(activityByDate, 
#                         mean = round(meanByDate$Mean,2), 
#                         median=medianByDate$Median)



# histogram 

hist(activityByDate$TotalSteps,
     main="Histogram : Total Steps per Day",
     xlab="Steps",
     col="blue",
     las = 1,
     cex.axis=0.7,  
     cex.names=0.7)
#breaks = length(activityByDate$TotalSteps))

# bar plot for total number of steps taken per day

# barplot1 <- barplot(activityByDate$TotalSteps, 
#                     names.arg= activityByDate$Date, 
#                     las= 2,
#                     cex.axis= 0.7,  
#                     cex.names= 0.7,
#                     ylim=c(0,25000),
#                     col="lightblue",
#                     main="Total Steps per Day")
# text(x = barplot1,y = activityByDate$TotalSteps, 
#      # y = paste("Total Steps : ", activityByDate$TotalSteps, 
#      #           "/ Mean : ", round(meanByDate$Mean, digits=2),
#      #           "/ Median : ", medianByDate$Median),
#      label = activityByDate$TotalSteps, 
#      cex = 0.65, col = "blue", srt=90, adj = c(-0.25,0.25))


# average daily activity pattern----


meanByInterval <- setNames(aggregate(activityData$steps, 
                                     list(activityData$interval), 
                                     mean),c("interval","Steps"))


plot(meanByInterval, type="l", 
     main= "Time series of Average steps for days", 
     las=1,
     xlab="Interval (mins)",
     ylab="Steps taken")


# maximum steps of the average daily activity pattern----

meanByInterval$interval[meanByInterval$Steps==(max(meanByInterval$Steps))]


# Imputing missing values ----

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


NAsCount <- length(NAs[NAs==TRUE])

print(NAsCount)

# strategy for filling in all of the missing values in the dataset.
# Use the mean from the time series data prepared earlier

activityImputNAs <- activityOriginal

library(dplyr)

AverageForNAs <- full_join(activityImputNAs, 
                           meanByInterval, 
                           by="interval" )[,4]



activityImputNAs$steps[NAs==TRUE] <- round(AverageForNAs[NAs],0)

# activityOriginal$Means <- meanByDate$Mean
# 
# activityOriginal$steps <- 


# sum ----
# Calculate the total number of steps taken per day

activityByDateNA <- setNames(aggregate(activityImputNAs$steps, 
                                       list(activityImputNAs$date), 
                                       sum),c("date","TotalSteps"))



# meadian and mean  ----
# Calculate and report the mean and median 
# of the total number of steps taken per day

summary(activityByDateNA$TotalSteps)[3]

summary(activityByDateNA$TotalSteps)[4]


#summary(activityByDate$TotalSteps)[3]

#summary(activityByDate$TotalSteps)[4]
# histogram 



hist(activityByDateNA$TotalSteps,
     main="Histogram : Total Steps per Day",
     xlab="Steps",
     col=rgb(0,0,1,1/4),
     las = 1,
     cex.axis=0.7,  
     cex.names=0.7
     )


hist(activityByDate$TotalSteps,
     main="Histogram : Total Steps per Day",
     xlab="Steps",
     col=rgb(1,0,0,1/4),
     las = 1,
     cex.axis=0.7,  
     cex.names=0.7,
     add=TRUE)



legend("topright", c("NAs Imputed", "NAs Excluded", "Both"), 
       fill=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5), rgb(1,0,1)))


#abline(h=mean(activityByDateNA$TotalSteps),col="black")

#abline(lm(height ~ bodymass))

#breaks = length(activityByDate$TotalSteps))


# average daily activity pattern----


meanByIntervalNA <- setNames(aggregate(activityImputNAs$steps, 
                                       list(activityImputNAs$interval), 
                                       mean),c("interval","Steps"))


plot(meanByIntervalNA, type="l", 
     main= "Time series: Average steps / days - NA Imputed", 
     las=1,
     xlab="Interval (mins)",
     ylab="Steps taken")


# maximum steps of the average daily activity pattern----

meanByInterval$interval[meanByInterval$Steps==(max(meanByInterval$Steps))]


# Seperate  activityData by weekdays and weekends----


activityDataWD <- weekdays(activityData$date)

activityDataWDLogical <- activityDataWD == "Saturday" | activityDataWD == "Sunday"


activityWeekend <- activityData[activityDataWDLogical,]

meanByWeekend <- setNames(aggregate(activityWeekend$steps, 
                                       list(activityWeekend$interval), 
                                       mean),c("interval","Steps"))

activityWeekday <- activityData[!activityDataWDLogical,]

meanByWeekday <-  setNames(aggregate(activityWeekday$steps, 
                                     list(activityWeekday$interval), 
                                     mean),c("interval","Steps"))

par(mfrow=c(1,2))


plot(meanByWeekend,
     type="l", 
     main= "Activity on Weekends", 
     las=1,
     xlab="Interval (mins)",
     ylab="Steps taken")

plot(meanByWeekday,
     type="l", 
     main= "and Weekdays", 
     las=1,
     xlab="Interval (mins)",
     ylab="Steps taken")



#####


WDcalday <- weekdays(activityData$date)

WDLogical <- WDcalday=="Saturday" | WDcalday=="Sunday"

activityData$wdfactor[WDLogical] <- "weekend"
activityData$wdfactor[!WDLogical] <- "weekday"


library(lattice)

activityData_Split <- aggregate(steps~interval+wdfactor, data = activityData, mean)

xyplot(steps ~ interval | wdfactor, data = activityData_Split, layout = c(1,2), type = "l")

 
activityWeekend <- activityData[activityData$weekday=="Saturday" | activityData$weekday=="Sunday", ]
activityWeekday <- activityData[activityData$weekday!="Saturday" & activityData$weekday!="Sunday", ]

meanByWeekend <- setNames(aggregate(activityWeekend$steps, 
                                    list(activityWeekend$interval), 
                                    mean),c("interval","Steps"))


meanByWeekday <- setNames(aggregate(activityWeekday$steps, 
                                    list(activityWeekday$interval), 
                                    mean),c("interval","Steps"))

par(mfcol=c(1,2))


plot(meanByWeekend,
     type="l", 
     main= "Activity on Weekends", 
     las=1,
     xlab="Interval (mins)",
     ylab="Steps taken")

plot(meanByWeekday,
     type="l", 
     main= "and Weekdays", 
     las=1,
     xlab="Interval (mins)",
     ylab="Steps taken")


