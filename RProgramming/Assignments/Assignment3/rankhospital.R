#setwd("C:\\Users\\Sham\\Documents\\DataScience\\R\\rprog_data_ProgAssignment3-data")
require("data.table")
rankhospital <- rankhospital <- function(state, outcome, num = "best") {
  if (!exists("outcome") | !(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
      stop("invalid outcome")
  }
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!exists("state") | !(state %in% unique(outcomeData$State))) {
     stop ("invalid state")
  } 
  order="asc"
  if(is.character(num)){
    rank=1
    if(num =="worst") {
      order="desc"
    }
  } else {
    rank=as.numeric(num)
  }
  stateOutcomeData = data.table(outcomeData[outcomeData$State == state,])
  stateOutcomeOrderedData = stateOutcomeData[order(stateOutcomeData[,2,with=FALSE])]
  if (order == "asc" ) {
    stateOutcomeRankedData = cbind( stateOutcomeOrderedData[,c(2,11,17,23),with=FALSE],apply(stateOutcomeOrderedData[,c(11,17,23),with=FALSE],2,function(x) rank(as.numeric(x), ties.method ="first")))
  } else {
    stateOutcomeRankedData = cbind( stateOutcomeOrderedData[,c(2,11,17,23),with=FALSE],apply(stateOutcomeOrderedData[,c(11,17,23),with=FALSE],2,function(x) rank(-as.numeric(x), ties.method ="first")))
  }
  setnames(stateOutcomeRankedData,  c("Hospital.Name","30DayMortHeartAttack","30DayMortHeartFailure", "30DayMortPneoumonia","heart attack", "heart failure", "pneumonia"))
  if(outcome == "heart attack") 
     bestHospital = stateOutcomeRankedData[stateOutcomeRankedData[,which(stateOutcomeRankedData$"heart attack" ==rank)]][,1,with=FALSE]
  if(outcome == "heart failure")
    bestHospital = stateOutcomeRankedData[stateOutcomeRankedData[,which(stateOutcomeRankedData$"heart failure" ==rank)]][,1,with=FALSE]
  if(outcome == "pneumonia")
    bestHospital = stateOutcomeRankedData[stateOutcomeRankedData[,which(stateOutcomeRankedData$"pneumonia" ==rank)]][,1,with=FALSE]
  if(nrow(bestHospital) == 0) {
    NA
  } else {
    as.character(bestHospital)
  }
   
}


