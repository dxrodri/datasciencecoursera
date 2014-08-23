#setwd("C:\\Users\\Sham\\Documents\\DataScience\\R\\rprog_data_ProgAssignment3-data")
require("data.table")
require(plyr)
rankall <- function(outcome, num = "best") {
  if (!exists("outcome") | !(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
      stop("invalid outcome")
  }
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeData[,11] <- as.numeric(outcomeData[,11])
  outcomeData[,17] <- as.numeric(outcomeData[,17])
  outcomeData[,23] <- as.numeric(outcomeData[,23])
  states =  data.table(sort(unique(outcomeData$State)))
  setnames(states, "state")
  order="asc"
  if(is.character(num)){
    rank=1
    if(num =="worst") {
      order="desc"
    }
  } else {
    rank=as.numeric(num)
  }
  outcomeData = data.table(outcomeData)
  #outcomeOrderedData = outcomeData[order(outcomeData[,2,with=FALSE])]
  outcomeOrderedData = outcomeData[order(outcomeData[,7, with=FALSE],outcomeData[,2,with=FALSE])]
  if (order == "asc" ) {
    #outcomeRankedData = cbind( outcomeOrderedData[,c(2,11,17,23),with=FALSE],apply(outcomeOrderedData[,c(11,17,23),with=FALSE],2,function(x) rank(as.numeric(x), ties.method ="first")))
    outcomeRankedData = cbind( outcomeOrderedData[,c(2,7,11,17,23),with=FALSE]
			,ave(outcomeOrderedData[,c(11),with=FALSE],outcomeOrderedData[,c(7),with=FALSE],FUN=function(x) rank(x, ties.method ="first"))
			,ave(outcomeOrderedData[,c(17),with=FALSE],outcomeOrderedData[,c(7),with=FALSE],FUN=function(x) rank(x, ties.method ="first"))
			,ave(outcomeOrderedData[,c(23),with=FALSE],outcomeOrderedData[,c(7),with=FALSE],FUN=function(x) rank(x, ties.method ="first")))
			      
  } else {
   outcomeRankedData = cbind( outcomeOrderedData[,c(2,7,11,17,23),with=FALSE]
			,ave(outcomeOrderedData[,c(11),with=FALSE],outcomeOrderedData[,c(7),with=FALSE],FUN=function(x) rank(-x, ties.method ="first"))
			,ave(outcomeOrderedData[,c(17),with=FALSE],outcomeOrderedData[,c(7),with=FALSE],FUN=function(x) rank(-x, ties.method ="first"))
			,ave(outcomeOrderedData[,c(23),with=FALSE],outcomeOrderedData[,c(7),with=FALSE],FUN=function(x) rank(-x, ties.method ="first")))
  }
  setnames(outcomeRankedData,  c("hospital","state","30DayMortHeartAttack","30DayMortHeartFailure", "30DayMortPneoumonia","heart attack", "heart failure", "pneumonia"))
  if(outcome == "heart attack") 
     bestHospital = outcomeRankedData[outcomeRankedData[,which(outcomeRankedData$"heart attack" ==rank)]][,c(1,2),with=FALSE]
  if(outcome == "heart failure")
    bestHospital = outcomeRankedData[outcomeRankedData[,which(outcomeRankedData$"heart failure" ==rank)]][,c(1,2),with=FALSE]
  if(outcome == "pneumonia")
    bestHospital = outcomeRankedData[outcomeRankedData[,which(outcomeRankedData$"pneumonia" ==rank)]][,c(1,2),with=FALSE]
  if(nrow(bestHospital) == 0) {
     NA
  } else {
    bestHospital = join(states,bestHospital,by=c("state"),type="left")
    bestHospital[,c(2,1),with=FALSE] 
  }
}


