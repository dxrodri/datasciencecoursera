complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	filenames=as.vector(sprintf("%03d.csv", id))
        data = lapply(file.path("specdata",filenames),read.csv)
        data = do.call(rbind.data.frame, data)
	#http://stackoverflow.com/questions/23634572/r-sum-complete-cases-in-one-column-grouped-by-or-sorted-by-a-value-in-another
        #nrow(head(na.omit(data[c("sulfate","nitrate","ID")])))
        
        #orders wrong from 30:25	
	#data$complete = complete.cases(data$sulfate, data$nitrate)
        #aggregate(complete ~ ID, data=data,FUN=sum)

	#does not return zero values
        #dt=data.table(data)
        #dt=dt[complete.cases(data$sulfate,data$nitrate),list(count = .N),by=ID]
	#setnames(dt,"ID","id")
	#setnames(dt,"count","nobs")
	#data.frame(dt)

	nobsNum <- numeric(0)
	for (cid in id) {
	  subdata = subset(data,ID==cid)
	  nobsNum <- c(nobsNum, nrow(na.omit(subdata)))
	}
	data.frame(id = id, nobs = nobsNum)
}
