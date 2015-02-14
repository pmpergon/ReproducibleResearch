## Reproducible Research Project 2

# libraries
library(ggplot2)
library (plyr)
library(reshape)


# input data (CASH = TRUE this section)
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp)
storm.data <- read.csv(bzfile(temp)) # takes a long time - big file!!
unlink(temp)

#       explore dataset TO ANSWER THE FOLLOWING TWO QUESTIONS

# 1.Across the United States, which types of events (as indicated in the EVTYPE variable) 
#       are most harmful with respect to population health?
# 2.Across the United States, which types of events have the greatest economic consequences?


dim(storm.data)
#                       [1] 902297     37
head(storm.data)

names (storm.data)
#  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"     "COUNTYNAME" "STATE"     
#  [8] "EVTYPE"     "BGN_RANGE"  "BGN_AZI"    "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END"
# [15] "COUNTYENDN" "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"      "F"         
# [22] "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP"
# [29] "WFO"        "STATEOFFIC" "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
# [36] "REMARKS"    "REFNUM" 
#               sellect variables of interest:
#                       EVTYPE; FATALITIES; INJURIES
#                       PROPDMGEXP; CROPDMEXP



# subset dataset to contain only desired variables

storm.data2 <- subset (storm.data, FATALITIES >0 | INJURIES>0, select= c(EVTYPE,FATALITIES, INJURIES, PROPDMGEXP,CROPDMGEXP))
head (storm.data2,3)

#       table of EVTYPE values
table(storm.data2$EVTYPE)
injuries.sum <- aggregate( INJURIES ~ EVTYPE, data=storm.data2, FUN=sum)
injuries.sumS <- injuries.sum[order(-injuries.sum$INJURIES),]
head(injuries.sumS,10)
fatalities.sum <- aggregate( FATALITIES ~ EVTYPE, data=storm.data2, FUN=sum)
fatalities.sumS <- fatalities.sum[order(-fatalities.sum$FATALITIES),]
head(fatalities.sumS,10)

merged.health <- merge_recurse(list(injuries.sum, fatalities.sum), all.x = TRUE)
merged.health <- mutate(merged.health, SUM = INJURIES+FATALITIES)
merged.healthS <- merged.health[order(-merged.health$SUM),]
health <- head(merged.healthS,10)


#       search for event type with maximum number of fatalities and of injuries

with(merged.health, EVTYPE[FATALITIES == max(FATALITIES)]) # [1] TORNADO
with(merged.health, EVTYPE[INJURIES == max(INJURIES)]) # [1] TORNADO

qplot(x=1:10, y=log10(SUM), data=health, fill=EVTYPE, geom="bar", 
      stat="identity", position="dodge", xlab="ten top fatality/injury events",
      ylab="log 10 of the sum of all injuries & fatalities") #from: http://www.r-bloggers.com/using-r-barplot-with-ggplot2/


