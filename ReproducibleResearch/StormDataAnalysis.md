# Human and Economic cost of major natural disasters in the United States
Sept 12, 2014  
##Synopsis
This report analyzes and calculates the human and economic cost of major natural disasters in the United State for the period 1960-2011 using data provided by the U.S. National Oceanic and Atmospheric Administration's (NOAA). This report tries  to answer the following two questions:

* Across the USA, what type of events are most harmful with respect to population health?
* Across the USA, what type of events have had the most dire economic consequences?

---

##Data Processing

### Loading and preprocessing the data
NOAA's storm data is downloaded from [storm data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) and saved in the working directly. R function read.csv is used to read the complete set.However, only the data required for this analysis is saved into memory for processing.


```r
  require(plyr)
  require(ggplot2)
  require(lattice)
  require(lubridate)
  require(gridExtra)
```


```r
data <- read.csv(bzfile("repdata-data-StormData.csv.bz2"),as.is=TRUE)  
stormData <- data[,c("EVTYPE", "BGN_DATE","MAG", "F", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP", "CROPDMG","CROPDMGEXP" )]
rm("data")
stormData$BGN_DATE<-as.Date(stormData$BGN_DATE,format="%m/%d/%Y %H:%M:%S")
stormData$eventYear<-year(stormData$BGN_DATE)
```

##### Convert property and crop damages to dollar amounts.
- Replace "K" for thousands, "M" for millions, and "B" for billions. +/-? as interpreted as 0. Damages are stored in CROPDMGAMT and PROPDMGAMT column in billions.


```r
stormData[stormData$PROPDMGEXP %in% c("","+","-","?"),"PROPDMGEXP"] <- 0
stormData[stormData$PROPDMGEXP %in% c("h","H"),"PROPDMGEXP"] <- 2
stormData[stormData$PROPDMGEXP %in% c("k","K"),"PROPDMGEXP"] <- 3
stormData[stormData$PROPDMGEXP %in% c("m", "M"),"PROPDMGEXP"] <- 6
stormData[stormData$PROPDMGEXP %in% c("b","B"),"PROPDMGEXP"] <- 9
stormData$PROPDMGAMT = stormData$PROPDMG * 10^as.numeric(stormData$PROPDMGEXP)/(10^9)
stormData[stormData$CROPDMGEXP %in% c("","+","-","?"),"CROPDMGEXP"] <- 0
stormData[stormData$CROPDMGEXP %in% c("h","H"),"CROPDMGEXP"] <- 2
stormData[stormData$CROPDMGEXP %in% c("k","K"),"CROPDMGEXP"] <- 3
stormData[stormData$CROPDMGEXP %in% c("m", "M"),"CROPDMGEXP"] <- 6
stormData[stormData$CROPDMGEXP %in% c("b","B"),"CROPDMGEXP"] <- 9
stormData$CROPDMGAMT = stormData$CROPDMG * 10^as.numeric(stormData$CROPDMGEXP)/(10^9)
```

##### Subsitute related events for major disasters only

```r
stormData[grep("^HURRICANE",stormData$EVTYPE),]$EVTYPE <- "HURRICANE"
stormData[grep("*FLOOD*",stormData$EVTYPE),]$EVTYPE <- "FLOOD"
```

### Exploratory analysis
##### Aggregate fatalies and injuries by disaster and year

```r
aggregatedHealthImpactStormData <- aggregate(cbind(FATALITIES,INJURIES)~EVTYPE+eventYear,data=stormData,FUN=sum)
```

##### Select top 10 events which cause maximum health impact across all years

```r
aggregatedTotalHealthImpactStormData <- aggregate(FATALITIES+INJURIES~EVTYPE,data=aggregatedHealthImpactStormData,FUN=sum)
colnames(aggregatedTotalHealthImpactStormData)[2] = "FATALITIES_AND_INJURIES"
topHealthImpactEvents <- head(aggregatedTotalHealthImpactStormData[order(-aggregatedTotalHealthImpactStormData$FATALITIES_AND_INJURIES),],5)[1]
```
- Top 5 natural disasters that have caused maximum health impact: TORNADO, FLOOD, EXCESSIVE HEAT, TSTM WIND, LIGHTNING

##### Aggregate fatalies and injuries by disaster and year

```r
aggregatedEconomicImpactStormData <- aggregate(cbind(PROPDMGAMT,CROPDMGAMT)~EVTYPE+eventYear,data=stormData,FUN=sum)
```
##### Select top 5 events which cause maximum health impact

```r
aggregatedTotalEconomicImpactStormData <- aggregate(PROPDMGAMT+CROPDMGAMT~EVTYPE,data=stormData,FUN=sum)
colnames(aggregatedTotalEconomicImpactStormData)[2] = "PROPDMGAMT_AND_CROPDMGAMT"
topEconomicImpactEvents <- head(aggregatedTotalEconomicImpactStormData[order(-aggregatedTotalEconomicImpactStormData$PROPDMGAMT_AND_CROPDMGAMT),],5)[1]
```
- Top 5 natural disasters that have caused maximum economic damage: FLOOD, HURRICANE, TORNADO, STORM SURGE, HAIL

### Data Presentation
#### Health Impact of major storms  in United States for the period 1960 - 2011

```r
majorHealthImpactStormData= subset(aggregatedHealthImpactStormData, EVTYPE %in% topHealthImpactEvents[,1])

p1 <-ggplot(majorHealthImpactStormData, aes(x=eventYear, y=FATALITIES, colour=EVTYPE)) + 
    geom_line(aes(group=EVTYPE)) +
    geom_point(size=1)+ theme(legend.position="bottom") +ylab("Total Fatalities")

p2 <-ggplot(majorHealthImpactStormData, aes(x=eventYear, y=INJURIES, colour=EVTYPE)) + 
    geom_line(aes(group=EVTYPE)) +
    geom_point(size=1) +  theme(legend.position="bottom") + ylab("Total Injuries")

p3 <-ggplot(majorHealthImpactStormData, aes(x=eventYear, y=FATALITIES+INJURIES, colour=EVTYPE)) + 
    geom_line(aes(group=EVTYPE)) +
    geom_point(size=1) +  theme(legend.position="bottom") + ylab("Fatalities and Injuries")

grid.arrange(p1,p2,p3,main="Fig 1.  Human Impact of major storms in USA for the period 1960 - 2011",nrow=2,as.table=TRUE)
```

![plot of chunk unnamed-chunk-9](./StormDataAnalysis_files/figure-html/unnamed-chunk-9.png) 

#### Economic Impact of major storms  in United States for the period 1960 - 2011

```r
majorEconomicImpactStormData= subset(aggregatedEconomicImpactStormData, EVTYPE %in% topEconomicImpactEvents[,1])

p5 <-ggplot(majorEconomicImpactStormData, aes(x=eventYear, y=PROPDMGAMT, colour=EVTYPE)) + 
    geom_line(aes(group=EVTYPE)) +
    geom_point(size=1)+ theme(legend.position="bottom") +ylab("Property Damage Cost")

p6 <-ggplot(majorEconomicImpactStormData, aes(x=eventYear, y=CROPDMGAMT, colour=EVTYPE)) + 
    geom_line(aes(group=EVTYPE)) +
    geom_point(size=1) +  theme(legend.position="bottom") + ylab("Crop Damage Cost")

p7 <-ggplot(majorEconomicImpactStormData, aes(x=eventYear, y=PROPDMGAMT +CROPDMGAMT, colour=EVTYPE)) + 
    geom_line(aes(group=EVTYPE)) +
    geom_point(size=1) +  theme(legend.position="bottom")  +ylab("Total Economic Cost")


grid.arrange(p5,p6,p7,main="Fig 2.  Economic Impact of major storms in USA for the period 1960 - 2011 (in Billions)",nrow=2,as.table=TRUE)
```

![plot of chunk unnamed-chunk-10](./StormDataAnalysis_files/figure-html/unnamed-chunk-10.png) 
---

##Results
#####Across the USA, what type of events are most harmful with respect to population health?
- As shown in the Figure 1 above, the top five natural disasters that are most harmful with respect to population are Tornadoes, Flood, Excessive Heat, Thunderstorm Wind and Lightning with Tornadoes causing most human casualities as well injuries over the given period.

#####Across the USA, what type of events have had the most dire economic consequences?
- As shown in the Figure 2 above, the top five natural disasters that have caused major economic consequences are floods, hurriances, tornadoes, storm surges and hail with floods causing the most economic damages in dollar amount. This report did not adjust dollar amounts for inflation.
