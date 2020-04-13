##Reading in the data, forked from course website
unzip("C:/Users/Herman/Documents/MDRP/RepData_PeerAssessment1/activity.zip")

MOVE<- read.csv("C:/Users/Herman/Documents/MDRP/RepData_PeerAssessment1/Activity.csv")

##WHAT IS THE TOTAL NUMBER OF STEPS PER DAY

      ##Looking at data set
      head(MOVE)
      names(MOVE)
      dim(MOVE)
      summary(MOVE)

      ##Calculating Total Number of Steps per Day
      TotalSPD<- tapply(MOVE$steps, MOVE$date, sum)

      ##Drawing Histogram
      qplot(TotalSPD, binwidth=500, main="Total number of steps per day")

      ##Calculating mean and median of the total no. of steps each day
      mean(TotalSPD, na.rm=TRUE)
      median(TotalSPD, na.rm=TRUE)

##WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN
      ##Calculate Average no. of steps for each interval
      mean_interval<-tapply(MOVE$steps, MOVE$interval, mean, na.rm=TRUE)

      ##Draw Time Series
      average<-as.data.frame(mean_interval)
      intervals <-as.data.frame(unique(MOVE$interval))
      df<-data.frame(average,intervals)
      colnames(df)<- c("average", "intervals")
      qplot(df$intervals, df$average, 
            geom="line", main="TIME SERIES", xlab="5-minute intervals", 
            ylab="Average steps")

      ##Finding which day contains the maximum number of steps
      df[which.max(df$average),]

##Imputing missing values
      ##calculate no. of missing values
      sum(is.na(MOVE$steps))
      ##Let missing value = mean for that interval
            #Subset data where there are no NAs
             MOVE_no_NA<- MOVE[which(!is.na(MOVE$steps)),]
            #Convert average per interval into intefers
             df$average<- as.integer(df$average)
            #Subset data where steps have NAs
             MOVE_NA<-MOVE[which(is.na(MOVE$steps)),]
            #Fill NAs with average steps based on interval
             MOVE_NA$steps<- ifelse(MOVE_NA$interval==df$intervals, df$average)
            #Row bind datasets such that NAs are replaced with means
             MOVE_impute<- rbind(MOVE_no_NA, MOVE_NA)
      ##Drawing historgram of total number of steps taken daily
      TotalSPD_impute<- tapply(MOVE_impute$steps, MOVE_impute$date, sum)
      qplot(TotalSPD_impute, binwidth=500, main="Total number of steps per day")
      ##Calculating mean and median      
      mean(TotalSPD_impute, na.rm=TRUE)
      median(TotalSPD_impute, na.rm=TRUE)
      
##DIFFERENCES IN ACTIVITY PATTERNS WEEKDAYS vs WEEKENDS
      ##Changing dates to days of the week
            
      
