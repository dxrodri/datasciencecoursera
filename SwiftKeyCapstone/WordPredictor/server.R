#setwd("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\WordPredictor")
require(shiny)
require(ggvis)
require(magrittr)
library(qdap)
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
library(dplyr) # Data preparation and pipes %>%.
library(plyr)
options("scipen"=100, "digits"=4)

#source("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\Assignment2_V3.r")
#source("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\predictFunctions_V1.r",local=TRUE)
source("predictFunctions_V1.r",local=TRUE)

useBigramForPrediction=TRUE
doStemming=FALSE
doStemCompletion=FALSE
removeStopWords=FALSE
#removeContractions=FALSE
expandContractions=TRUE
removeUnusedObjects=FALSE
saveGramsToFile=FALSE
sparsePercentage=0.50
myStopWords <-  c(expand_contractions(stopwords("english")))

dataLoadComplete=FALSE
print("loading data")
freqGrams=loadGramsFromFile()
print("build word frames")
wordFrameGrams=buildWordFrames(freqGrams)
SGT=buildSGT(freqGrams)
dataLoadComplete=TRUE
print("Data tables built")
#https://groups.google.com/forum/#!topic/shiny-discuss/sE3iChBRlTI
#http://spark.rstudio.com/johnharrison/shinyBS-Demo/
#https://groups.google.com/forum/#!topic/shiny-discuss/09UoBQEoCv8
#http://shiny.rstudio.com/articles/selectize.html
#http://chrisbeeley.net/website/shinytalk/jQuery.html
#http://shiny.rstudio.com/gallery/selectize-examples.html
#http://www.inside-r.org/packages/cran/shiny/docs/updateTextInput

shinyServer(function(input, output, session) {

  observe({
    print(paste("in observe", input$sentence))
    if(nchar(input$sentence)>0) {
      updateTextInput(session, "sentencez", value = paste(trim(input$sentence), dataInput()[1]))
    } else {
      updateTextInput(session, "sentencez", value = "")
    }
  })

  dataInput <- reactive({
     print(paste("Data load", dataLoadComplete))
     if(dataLoadComplete) {
       print(paste("in reactive..", input$sentence))
       nextWords <- getNextWords(wordFrameGrams,SGT, input$sentence,5)
       print(paste("Next words [",nextWords,"]",sep = ""))
       nextWords
     } else {
       return(c("Loading data... Please wait"))
     }
 })
 output$nextWords <- renderText(dataInput())
 #output$sentencex <- renderText(paste(input$sentence, trim(dataInput()[1])))
 #output$sentencexy <- renderText(paste(input$sentence, trim(dataInput()[1])))
  })

# runApp("DP_Assignment1")
