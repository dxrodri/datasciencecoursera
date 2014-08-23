corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

	#Init <- function(workDirStr = "C:/Users/denbrige/100 FxOption/103 FxOptionVerBack/080 Fx Git/R-source") {
        #setwd(workDirStr)
        #}


	#filenames = list.files(directory)
        #subset(dt,nobs>threshold)
	#setwd("C:\\Users\\Sham\\Documents\\DataScience\\R")
        #source("complete.R")
        #dt <- complete("specdata", id = 1:332)
	corrVector <- numeric(0)
	filenames = list.files(directory)
	data = lapply(file.path("specdata",filenames),read.csv)
        data = do.call(rbind.data.frame, data)

	#does not return zero values
        dt=data.table(data)
        dt=dt[complete.cases(data$sulfate,data$nitrate),list(count = .N),by=ID]
	setnames(dt,"ID","id")
	setnames(dt,"count","nobs")

	dt= dt[dt$nobs>threshold]
	for (cid in dt$id) {
	   subdata = subset(data,ID==cid)
	   corrVector = c(corrVector,cor(subdata$sulfate, subdata$nitrate,use="pairwise.complete.obs"))
	}
       return (corrVector)


	#nobsNum <- numeric(0)
	#for (cid in id) {
	#  subdata = subset(data,ID==cid)
	#  nobsNum <- c(nobsNum, nrow(na.omit(subdata)))
	#}
	#df = data.frame(id = id, nobs = nobsNum)
        #dt = data.table(df)
        #dt= dt[dt$nobs>threshold]
	#for (cid in dt$id) {
	#   subdata = subset(data,ID==cid)
	#   corrVector = c(corrVector,cor(subdata$sulfate, subdata$nitrate,use="pairwise.complete.obs"))
	#}
       #return (round(corrVector,5))
}
