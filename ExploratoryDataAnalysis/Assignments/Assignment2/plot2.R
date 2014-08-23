setwd("C:\\Users\\Sham\\Documents\\DataScience\\ExploratoryDataAnalysis\\")
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008? Use the base plotting system to make a plot answering this question.

NEI <- readRDS("exdata_data_NEI_data\\summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data\\Source_Classification_Code.rds")
NEI_dt = data.table(NEI)
baltimoreEmissionsData = NEI_dt[NEI_dt$fips == "24510", list(totalEmissions = sum(Emissions)), by = "year"]
png('plot2.png')
plot(totalEmissions~year,xlab="years", ylab="totalEmissions", main = " plot1 - total emissions per year in Baltimore, Maryland", data=baltimoreEmissionsData, type="l",  col="red")
dev.off()
#colnames(NEI)
#[1] "fips"      "SCC"       "Pollutant" "Emissions" "type"      "year"   
#head(NEI)
#    fips      SCC Pollutant Emissions  type year
#4  09001 10100401  PM25-PRI    15.714 POINT 1999
#8  09001 10100404  PM25-PRI   234.178 POINT 1999
#12 09001 10100501  PM25-PRI     0.128 POINT 1999
#16 09001 10200401  PM25-PRI     2.036 POINT 1999
#20 09001 10200504  PM25-PRI     0.388 POINT 1999
#24 09001 10200602  PM25-PRI     1.490 POINT 1999



#colnames(SCC)
# [1] "SCC"                 "Data.Category"       "Short.Name"          "EI.Sector"           "Option.Group"        "Option.Set"          "SCC.Level.One"      
# [8] "SCC.Level.Two"       "SCC.Level.Three"     "SCC.Level.Four"      "Map.To"              "Last.Inventory.Year" "Created_Date"        "Revised_Date"       
#[15] "Usage.Notes"    

#    SCC Data.Category                                                                 Short.Name                              EI.Sector Option.Group Option.Set
#1 10100101         Point                   Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal Fuel Comb - Electric Generation - Coal                        
#2 10100102         Point Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker Fuel Comb - Electric Generation - Coal                        
#3 10100201         Point       Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Wet Bottom Fuel Comb - Electric Generation - Coal                        
#4 10100202         Point       Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Dry Bottom Fuel Comb - Electric Generation - Coal                        
#5 10100203         Point                   Ext Comb /Electric Gen /Bituminous Coal /Cyclone Furnace Fuel Comb - Electric Generation - Coal                        
#6 10100204         Point                   Ext Comb /Electric Gen /Bituminous Coal /Spreader Stoker Fuel Comb - Electric Generation - Coal                        
#                SCC.Level.One       SCC.Level.Two               SCC.Level.Three                                SCC.Level.Four Map.To Last.Inventory.Year Created_Date
#1 External Combustion Boilers Electric Generation               Anthracite Coal                               Pulverized Coal     NA                  NA             
#2 External Combustion Boilers Electric Generation               Anthracite Coal             Traveling Grate (Overfeed) Stoker     NA                  NA             
#3 External Combustion Boilers Electric Generation Bituminous/Subbituminous Coal Pulverized Coal: Wet Bottom (Bituminous Coal)     NA                  NA             
#4 External Combustion Boilers Electric Generation Bituminous/Subbituminous Coal Pulverized Coal: Dry Bottom (Bituminous Coal)     NA                  NA             
#5 External Combustion Boilers Electric Generation Bituminous/Subbituminous Coal             Cyclone Furnace (Bituminous Coal)     NA                  NA             
#6 External Combustion Boilers Electric Generation Bituminous/Subbituminous Coal             Spreader Stoker (Bituminous Coal)     NA                  NA             


