require(ggplot2)
require(plyr)
require(data.table)
setwd("C:\\Users\\Sham\\Documents\\DataScience\\ExploratoryDataAnalysis\\")
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time in motor vehicle emissions?

NEI <- readRDS("exdata_data_NEI_data\\summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data\\Source_Classification_Code.rds")
NEI_dt = data.table(NEI)
SCC_dt  = data.table(SCC)
motorVehiclesGroup <- data.table(SCC[grep("^Mobile(.)*Vehicles",SCC$"EI.Sector"),])
baltimoreAndLAEmissionsData = NEI_dt[NEI_dt$fips %in% c("24510", "06037"), ]

joinedData = join(baltimoreAndLAEmissionsData, motorVehiclesGroup, by=c("SCC"),type="inner")

#joinedEmissionsData = joinedData[, list(totalEmissions = sum(Emissions)), by = c("EI.Sector","fips","year")]
joinedEmissionsData = joinedData[, list(totalEmissions = sum(Emissions)), by = c("fips","year")]
png('plot6.png')
p6 = ggplot(data=joinedEmissionsData, aes(x=year, y=totalEmissions, colour=fips)) 
p6 = p6 + geom_line(aes(group=fips)) +  geom_point(size=3)
p6 = p6 + labs(title="plot6 -  compare vehicle total emissions in Baltimore and LA")
p6 = p6 + scale_fill_continuous(guide = guide_legend()) + theme(legend.direction = "vertical", legend.position="bottom")
p6
dev.off()
#colnames(NEI)
