## REPRODUCIBLE RESEARCH - Project 1.2

## Libraries
library (data.table)
library( plyr)
library(lattice)
library(ggplot2)
library(grid)
library(gridExtra)
library(xtable)

##  workspace directory settings
WD <- getwd()
dir <-"./ReproducibleResearch"


## Data Loading
filename <- "./ReproducibleResearch/activity.csv"
activity <- read.csv(filename)
summary(activity)

## Data Porcessing
#      removing NA
activity1 <- activity[complete.cases(activity),]
summary (activity1)

#               histogram of the total number of steps taken each day per interval
histogram(~steps|date, data=activity,layout=c(7,9), par.strip.text=list(cex=.75))
histogram(~log10(steps)|date, data=activity,layout=c(7,9), par.strip.text=list(cex=.75))

## Data Anlysis
#       Calculate the total number of steps taken per day
plot.new()
par("mar")
steps.day <- aggregate(activity1$steps,by=list(activity1$date),sum)
steps<- steps.day$x
doy<-yday(steps.day$Group.1)
b<- barplot(steps, main="total steps per day", 
            sub="from October 2nd to November 29th 2012", xlab="day of the year", ylab="steps")
# aligning axis with barplot output, solution suggested in 
# http://stackoverflow.com/questions/8285759/r-barplot-axis-scaling by Ben Bolker
axis(1, at=b[(1:53)], label= doy, tick=TRUE)

#       steps per day - sum of interval steps per day:


#       histogram of the total number of steps taken each day
plot.new()
par("mar")
hist(log10(steps.day$x), breaks=30, col="yellow", main = "Number of steps per day", xlab="log10 of number of steps", ylab="frequencey")
hist(steps.day$x, breaks=30, col="orange", main = "Number of steps per day", xlab="number of steps", ylab="frequencey")

#       mean & median 
# for steps (activity) per interval
# dt <- data.table(activity1)
# mean.activities <- dt[,list(mean=mean(steps),median=as.double(median(steps))),by=date] 
# mean.activities
# barchart(~mean|date, data=mean.activities, layout=c(7,9), par.strip.text=list(cex=.75))



hist(activity1$steps, breaks=30,main = "Number of steps", xlab="log10 of number of steps", ylab="frequencey")


#       Calculation of mean total number of steps taken per day?
#       mean & median for steps (activity) per day
mean.activity <- mean(steps.day$x)
sd.activity <- sd(steps.day$x)
perdev.activity <- 100*sd.activity/mean.activity
median.activity <- median(steps.day$x)
mean.steps <- mean (activity1$steps,na.rm=TRUE)
sd.steps <- sd(activity1$steps)
perdev.steps <- 100*sd.steps/mean.steps
median.steps <- median(activity1$steps,na.rm=TRUE)

df<- data.frame(mean.activity,sd.activity,median.activity,mean.steps,median.steps)
colnames(df) <-c("- mean per day","+/- sdev per day","- median per day", "- mean per interval", "-median per interval-")
df
table1 <- xtable(df)
print (table1, type="html")

#  What is the average daily activity pattern?
daily.activity <- aggregate(steps~interval, data=activity1, mean) #http://stackoverflow.com/questions/14924731/r-calculate-the-mean-value-of-a-variable-by-unique-values-of-another-variable-i
#      summary(daily.activity)
daily.activity <- mutate(daily.activity, 
                         interval=(interval/100))
# interval=((interval/100)+100*(((interval/100)%%1)/60)))

x<- which.max(daily.activity$steps)

#       Plot of average daily activity pattern?
plot.new()
plot (daily.activity$interval, daily.activity$steps, type="l", 
      main =" daily activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))

#       5 minute interval with max number of steps 
daily.activity[x,]


# Imputing missing values                       

number.na <- sum(is.na(activity$steps))

# total mean imputation; http://thomasleeper.com/Rcourse/Tutorials/NAhandling.html
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- mean (activity2$steps, na.rm=TRUE)

# random sampling imputation

activity3 <- activity
samples <- activity3$steps[!is.na(activity3$steps)]
activity3$steps[is.na(activity3$steps)] <- sample(activity3$steps[!is.na(activity3$steps)],
                                                  sum(is.na(activity3$steps)), TRUE)
#       Outputs of Imputing missing values section

number.na

summary (activity3)
summary (activity)

#compare original with random imuted value only (activity vs. activity3 values)
# possible to compare side by side?
steps.day3 <- aggregate(activity3$steps,by=list(activity3$date),sum)

steps.day$method <- "none"
steps.day3$method <- "RVS imputed"

comparison <- rbind(steps.day,steps.day3)
ggplot(comparison, aes((x), fill=method)) + geom_density(alpha=.2)



# possible to invert sequence? (lattice)
histogram(~log10(steps)|date, data=activity,layout=c(7,9), main="Original Dataset", par.strip.text=list(cex=.75))
histogram(~log10(steps)|date, data=activity3,layout=c(7,9), main="Random Value Imputed Dataset", par.strip.text=list(cex=.75))
histogram(~log10(steps)|date, data=activity2,layout=c(7,9), main="Mean Imputed Dataset", par.strip.text=list(cex=.75))

# use also lattice plot for comparing
hist(log10(activity3$steps), main = "Number of steps", xlab="log10 of number of steps", ylab="frequencey")
hist(log10(activity$steps), main = "Number of steps", xlab="log10 of number of steps", ylab="frequencey")


summary (activity)
summary (activity3)
steps.day3 <- aggregate(activity3$steps,by=list(activity3$date),sum)
mean.activity3 <- mean(steps.day3$x)
sd.activity3<- sd(steps.day3$x)
median.activity3 <- median(steps.day3$x)
df2<- data.frame(mean.activity,sd.activity,mean.activity3,sd.activity3)
colnames(df2) <-c("- mean per day","+/- sdev per day","- imputetd mean per day","+/- sdev per day -")
df2

# Are there differences in activity patterns between weekdays and weekends?
activity3$date <- as.POSIXlt(activity3$date)
activity3$day <-weekdays(as.Date(activity3$date))
activity3$day <- factor(activity3$day, levels= c("Monday", 
                                                 "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weekly.steps <- by(activity3$steps, as.factor(activity3$day), mean)


#       Are there differences in activity patterns between weekdays and weekends?

plot(weekly.steps, main = "Number of steps per interval by day of the week", 
     ylab="mean number of steps", xlab="day of the week", type='l', xaxt='n', cex=0.65)
axis(1, at = 1:7, labels=c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), cex.axis=0.65)
abline(h=mean(weekly.steps), col=3, lty=2)


#### What is the average Wednesday trend per interval along the day? Is it different 
### form the Friday and saturday and Sunday trends? Is ther a particular time of the
### day for the elevated count?

activity3$date <- as.POSIXlt(activity3$date)
activity3$day <-weekdays(as.Date(activity3$date))
activity3$day <- factor(activity3$day, levels= c("Monday", 
                                                 "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
activity3$day
head(activity3)

selection <- c("Monday","Tuesday","Thursday")

Monday.steps <- subset( activity, activity3$day =="Monday")
Monday.activity <- aggregate(steps~interval, data=Monday.steps, mean)
Monday.activity <- mutate(Monday.activity,interval=(interval/100))

Tuesday.steps <- subset( activity, activity3$day =="Tuesday")
Tuesday.activity <- aggregate(steps~interval, data=Tuesday.steps, mean)
Tuesday.activity <- mutate(Tuesday.activity,interval=(interval/100))

Wednesday.steps <- subset( activity, activity3$day =="Wednesday")
Wednesday.activity <- aggregate(steps~interval, data=Wednesday.steps, mean)
Wednesday.activity <- mutate(Wednesday.activity,interval=(interval/100))

Thursday.steps <- subset( activity, activity3$day =="Thursday")
Thursday.activity <- aggregate(steps~interval, data=Thursday.steps, mean)
Thursday.activity <- mutate(Thursday.activity,interval=(interval/100))

Friday.steps <- subset( activity, activity3$day =="Friday")
Friday.activity <- aggregate(steps~interval, data=Friday.steps, mean)
Friday.activity <- mutate(Friday.activity,interval=(interval/100))

Saturday.steps <- subset( activity, activity3$day =="Saturday")
Saturday.activity <- aggregate(steps~interval, data=Saturday.steps, mean)
Saturday.activity <- mutate(Saturday.activity,interval=(interval/100))

Sunday.steps <- subset( activity, activity3$day =="Sunday")
Sunday.activity <- aggregate(steps~interval, data=Sunday.steps, mean)
Sunday.activity <- mutate(Sunday.activity,interval=(interval/100))

#plot togather in a column:
#       http://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
par("mar")
# change margins
plot.new()

par (mar=c(1,1,1,1))
par(mfrow = c(5, 2), cex=.5)

plot (Monday.activity$interval, Monday.activity$steps, type="l", ylim=c(0,350), col="orange",
      main =" Monday activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))
plot (Friday.activity$interval, Friday.activity$steps, type="l", ylim=c(0,350), col="blue",
      main =" Friday activity",xlab="hours", ylab="average number of steps",
      xaxt='n') 
axis(1,at = c(0,6,12,18,24))
plot (Tuesday.activity$interval, Tuesday.activity$steps, type="l", ylim=c(0,350),col="orange",
      main ="Tuesday activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))
plot (Saturday.activity$interval, Saturday.activity$steps, type="l", ylim=c(0,350), col="blue",
      main =" Saturday activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))
plot (Wednesday.activity$interval, Wednesday.activity$steps, type="l", ylim=c(0,350),col="orange",
      main ="Wednesday activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))
plot (Sunday.activity$interval, Sunday.activity$steps, type="l", ylim=c(0,350), col="blue",
      main =" Sunday activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))
plot (Thursday.activity$interval, Thursday.activity$steps, type="l", ylim=c(0,350),col="orange",
      main ="Thursday activity",xlab="hours", ylab="average number of steps",
      xaxt='n')
axis(1,at = c(0,6,12,18,24))

# boxplots of steps per day 

qplot(interval, log10(steps), data=activity3, geom="boxplot", facets=.~day, color=day, alpha=.5 )

# Collect values for Monday through Thursday versus Friday through Sunday  !!!!
# and report as a box plot
selection <- c("Monday","Tuesday","Wednesday","Thursday")
Weekday.steps <- subset( activity, activity3$day == selection)
Weekday.mean.steps <- aggregate(Weekday.steps$steps,by=list(Weekday.steps$date),sum)
mean.weekday <- mean(Weekday.steps$steps, na.rm=TRUE)

mean.Weekday.day <- mean(aggregate(Weekday.steps$steps,by=list(Weekday.steps$date),sum)$x, na.rm=TRUE)
sd.Weekday.day <- sd(aggregate(Weekday.steps$steps,by=list(Weekday.steps$date),sum)$x, na.rm=TRUE)

selection2 <- c("Friday","Saturday","Sunday")
Weekend.steps <- subset( activity, activity3$day == selection2)
Weekend.mean.steps <- aggregate(Weekend.steps$steps,by=list(Weekend.steps$date),sum)
mean.weekend <- mean(Weekend.steps$steps, na.rm=TRUE)

mean.Weekend.day <- mean(aggregate(Weekend.steps$steps,by=list(Weekend.steps$date),sum)$x, na.rm=TRUE)
sd.Weekend.day <- sd(aggregate(Weekend.steps$steps,by=list(Weekend.steps$date),sum)$x, na.rm=TRUE)


p1<- qplot(Group.1, x, data=Weekday.mean.steps, geom="boxplot", ylim=c(0,7000), alpha=.5, main="weekdays" )
p1 <- p1 + theme(axis.title.x=element_blank()) + geom_hline(aes(yintercept=mean.Weekday.day))

p2 <-qplot(Group.1, x, data=Weekend.mean.steps, geom="boxplot", ylim=c(0,7000), alpha=.5, main="weekend" )
p2 <- p2 + theme(axis.title.x=element_blank()) + geom_hline(aes(yintercept=mean.Weekend.day))
grid.arrange(p1,p2,ncol=2,main="comparison between weekdays and weekend log10 of steps")

df3<- data.frame(mean.weekday,mean.weekend)
colnames(df3) <-c("- mean per interval per weekday","- mean per interval per weekend -")
df3

df4<- data.frame(mean.Weekday.day,sd.Weekday.day,mean.Weekend.day,sd.Weekend.day)
colnames(df4) <-c("- mean per day per weekday"," +/- SD","- mean per day per weekend"," +/- SD -")
df4

## Return to original settings

