#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

setwd("C:\\Users\\Sham\\Documents\\DataScience\\ExploratoryDataAnalysis\\")
NEI <- readRDS("exdata_data_NEI_data\\summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data\\Source_Classification_Code.rds")

emissionsData <- aggregate(NEI[,c("Emissions")],by=list(NEI$year),"sum")
colnames(emissionsData) = c("year", "totalEmissions")
png('plot1.png')
plot(totalEmissions~year,xlab="years", ylab="totalEmissions", main = " plot1 - total emissions per year", data=emissionsData, type="l")
dev.off()
