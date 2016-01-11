library(dplyr)
library(ggplot2)

#load the dataset activity.csv to variable activity
activity<- tbl_df(read.csv("./activity.csv",header=TRUE, na.strings = "NA"))

#convert to date class
activity$date<-as.character(activity$date)
activity$date<-as.Date(activity$date,"%Y-%m-%d")
activity$date<- as.POSIXct(activity$date)

#subset non na data
non_na_data <- activity %>%
  filter(!is.na(steps))

#get total steps by date
steps_day <- non_na_data %>%
  group_by(date) %>%
  summarise(total_steps=sum(steps))

#plot histogram of total steps
png(filename ="./figures/histogram of steps per day.png",bg="transparent")
hist(steps_day$total_steps, xlab="total steps per day", main="Total steps per day",col="blue")
dev.off()

#print mean and median of total steps
mean_steps <- mean(steps_day$total_steps)
print(paste("Mean Steps :",mean_steps))
median_steps <- median(steps_day$total_steps)
print(paste("Median Steps :",median_steps))

#summarize mean number of steps by interval 
time_series_steps <- non_na_data %>%
  group_by(interval) %>%
  summarise(avg_steps=mean(steps))

#plot time series data
png(filename ="./figures/time series of average steps per day.png",bg="transparent")
plot(time_series_steps$interval,
     time_series_steps$avg_steps,
     xlab="Interval",
     ylab="Average steps per Day",
     main=" Time Series of average steps per day",
     col="red",
     type="l")
dev.off()

#Interval with greatest average steps
greatest_step<- time_series_steps[time_series_steps$avg_steps==max(time_series_steps$avg_steps),]$interval
print(paste("Interval with greatest average steps :",greatest_step))

#print number of NA data
number_of_na<-sum(is.na(activity$steps))
print(paste("Number of NAs :",number_of_na))

#fill NA data with mean of each time interval
list_of_na_intervals <- as.data.frame(activity$interval[is.na(activity$steps)])
names(list_of_na_intervals)[1]<-"interval"
list_of_na_intervals <- left_join(list_of_na_intervals,time_series_steps,by="interval")
activity$steps[is.na(activity$steps)]<- list_of_na_intervals$avg_steps

#summarize sum steps by date
steps_day_with_na <- activity %>%
  group_by(date) %>%
  summarise(total_steps=sum(steps))

#plot histogram
png(filename ="./figures/histogram of steps per day with replaced NAs.png",bg="transparent")
hist(steps_day_with_na$total_steps, xlab="total steps per day", main="Total steps per day with replaced NAs",col="blue")
dev.off()

#print median and mean of total steps
mean_steps_with_na <- mean(steps_day_with_na$total_steps)
print(paste("Mean Steps with NA :",mean_steps_with_na))
median_steps_with_na <- median(steps_day_with_na$total_steps)
print(paste("Median Steps with NA :",median_steps_with_na))

#add new variable day to show weekday or weekend
activity$day=ifelse(weekdays(activity$date)%in%c("Saturday","Sunday"),"weekend","weekday")
activity$day=factor(activity$day,levels=c("weekday","weekend"))

#summarize mean steps by interval/day
time_series_week <- activity %>%
  group_by(interval,day) %>%
  summarise(avg_steps=mean(steps))

#plot panel plot
png(filename ="./figures/weekday vs weekend comparison.png",bg="transparent")
print(ggplot(time_series_week,
       aes(interval,avg_steps))+
       geom_line(color="aquamarine4",lwd=0.5)+
       facet_wrap(~day, nrow=2)+
       ylab("Average Steps"))
dev.off()

