require(plyr)
require(data.table)
setwd("C:\\Users\\Sham\\Documents\\DataScience\\GettingAndCleaningData")
xTrainData = read.table("UCI HAR Dataset\\train\\X_train.txt", sep="")
yTrainData = read.table("UCI HAR Dataset\\train\\Y_train.txt", sep="")

#head(xTrainData[,1:3]) --> returns 3 columns, 6 rows
#class(xTrainData)
#xTrainData_dt =  data.table(xTrainData)
#head(xTrainData_dt[,1:3])  returns 1 2 3
#head(xTrainData_dt[,1:3, with=FALSE])

xTestData = read.table("UCI HAR Dataset\\test\\X_test.txt", sep="")
yTestData = read.table("UCI HAR Dataset\\test\\Y_test.txt", sep="")
features = read.table("UCI HAR Dataset\\features.txt",sep="")
activityLabels = read.table("UCI HAR Dataset\\activity_labels.txt",sep="")

names(xTrainData) = features[,2]
names(xTestData)  = features[,2]
xData = rbind(xTestData,xTrainData)
yData  = rbind(yTestData,yTrainData)
yData = join(yData,activityLabels)
colnames(yData) = c("activityId", "activity")
#meanColumns = grep("*[m|M]ean*",features[,2])
#xDataFiltered = xData[,meanColumns]
meanAndStdColumns =  grep("(*std*)|(*[M|m]ean*)",features[,2])
xDataFiltered = xData[,meanAndStdColumns ]
mergedFilteredData = data.table(cbind(yData,xDataFiltered))
#testData = head(mergedFilteredData[,1:5])
#testData[,lapply(.SD,mean),by=c("seqId","activity")]
averageData = mergedFilteredData[,lapply(.SD,mean),by=c("activityId","activity")]
averageData = averageData[order(activityId)]
write.table(averageData ,file= "averageData.csv" ,row.names=FALSE)

