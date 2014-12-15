set.seed(1000)
#rm(list=ls())
#gc()
setwd("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\data\\Coursera-SwiftKey")

#ref: TextMiningO.pdf 
library(tm) # Framework for text mining.
library(SnowballC) # Provides wordStem() for stemming.
#library(qdap) # Quantitative discourse analysis of transcripts.
#library(qdapDictionaries)
library(dplyr) # Data preparation and pipes %>%.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2)
Sys.setenv(JAVA_HOME="")
options( java.parameters = "-Xmx4g" )
library(rJava)
library(RWeka)
library(wordcloud)
library(qdap)
library(openNLP)
library(plyr)

useBigramForPrediction=TRUE
doStemming=FALSE
doStemCompletion=FALSE
removeStopWords=FALSE
#removeContractions=FALSE
expandContractions=TRUE
removeUnusedObjects=FALSE
saveGramsToFile=FALSE
sparsePercentage=0.50

options("scipen"=100, "digits"=4)
source("C:\\Users\Sham\\Documents\\DataScience\\capstone\\Assignments\\WordPredictor\\predictFunctions_V1.r")

#https://code.google.com/p/stop-words
#stoplist<-scan(file='stop-words_english_3_en.txt',
#                  what='character',
#                  strip.white=TRUE,
#                  blank.lines.skip=TRUE,
#                  fileEncoding='CP1251',
#                  encoding='CP1251')

#code.google.com/p/badwordslist/downloads/detail?name=badwords.txt
#badwordslist<-scan(file='badwords.txt',
#                  what='character',
#                  strip.white=TRUE,
#                  blank.lines.skip=TRUE,
#                  fileEncoding='CP1251',
#                  encoding='CP1251')

#myStopWords <- c("i","me","my","we","our","ours", "you","your","and" , "are", "for" , "in" , "is" , "was", "it " , "not" , "the" , "to")

myStopWords <-  c(expand_contractions(stopwords("english")))
#idx <- which(myStopWords == "of")  #case of beer
#myStopWords <- myStopWords[-idx]
#myStopWords <- setdiff(myStopWords, c('r', 'big'))

getCorpusData <- function(cname, filename) {
   corpusData <- VCorpus(DirSource(cname,pattern=filename),readerControl=list(reader=readPlain,language="english",encoding='ANSI'))
   print("set_detect")
   corpusData <- tm_map(corpusData, sent_detect)
   corpusData <- tm_map(corpusData, tolower)
   corpusData <- tm_map(corpusData, removeNumbers)
   print("expanding")
   if(expandContractions) {
     corpusData <- tm_map(corpusData, expand_contractions)
   }
   corpusData <- tm_map(corpusData, removePunctuation, preserve_intra_word_dashes = TRUE)
   print("removingstopwords")
   if(removeStopWords) {
     corpusData <- tm_map(corpusData, removeWords, myStopWords)
   }
   corpusData <- tm_map(corpusData, stripWhitespace)
   corpusData <- tm_map(corpusData, trim)
   corpusData <- tm_map(corpusData,PlainTextDocument)
   if(doStemming) {
     corpusData <- tm_map(corpusData, stemDocument,language="english")
   }
   return(corpusData)
}



getWordFrameGrams <- function() {
  cname <-file.path(".","corpus_sample10_1")
  corpusData.twitter = getCorpusData(cname,"en_US.twitter_sample10_1.txt")
  corpusData.news = getCorpusData(cname,"en_US.news.txt_sample10_1.txt")
  corpusData.blogs = getCorpusData(cname,"en_US.blogs_sample10_1.txt")


  UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
  QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

  getDocumentTermMatrix <- function(corpusData, tokenizer,minWordLength, maxWordLength) {
    corpusTDMGrams <- TermDocumentMatrix(corpusData, control = list(wordLengths = c(minWordLength, maxWordLength), tokenize = tokenizer))
    corpusTDMGrams <- removeSparseTerms(corpusTDMGrams,sparsePercentage)
    return(corpusTDMGrams)
  }

  minWordLength=2  #1*2 
  maxWordLength=15  #1*15 
  corpusTDMGrams.twitter.unigrams <- getDocumentTermMatrix(corpusData.twitter,UnigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.news.unigrams <- getDocumentTermMatrix(corpusData.twitter,UnigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.blogs.unigrams <- getDocumentTermMatrix(corpusData.twitter,UnigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.unigrams <- c(corpusTDMGrams.twitter.unigrams,corpusTDMGrams.news.unigrams,corpusTDMGrams.blogs.unigrams)

  minWordLength=5  #2*2 + 1 spaces
  maxWordLength=31  #2*15 +1 spaces
  corpusTDMGrams.twitter.bigrams <- getDocumentTermMatrix(corpusData.twitter,BigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.news.bigrams <- getDocumentTermMatrix(corpusData.twitter,BigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.blogs.bigrams <- getDocumentTermMatrix(corpusData.twitter,BigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.bigrams <- c(corpusTDMGrams.twitter.bigrams,corpusTDMGrams.news.bigrams,corpusTDMGrams.blogs.bigrams)

  minWordLength=8  #3*2 + 2 spaces
  maxWordLength=47  #3*15 +2 spaces
  corpusTDMGrams.twitter.trigrams <- getDocumentTermMatrix(corpusData.twitter,TrigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.news.trigrams <- getDocumentTermMatrix(corpusData.twitter,TrigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.blogs.trigrams <- getDocumentTermMatrix(corpusData.twitter,TrigramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.trigrams <- c(corpusTDMGrams.twitter.trigrams,corpusTDMGrams.news.trigrams,corpusTDMGrams.blogs.trigrams)

  minWordLength=9  #4*2 + 1 spaces
  maxWordLength=63  #4*15 +3 spaces
  corpusTDMGrams.twitter.quadgrams <- getDocumentTermMatrix(corpusData.twitter,QuadgramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.news.quadgrams <- getDocumentTermMatrix(corpusData.twitter,QuadgramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.blogs.quadgrams <- getDocumentTermMatrix(corpusData.twitter,QuadgramTokenizer,minWordLength, maxWordLength)
  corpusTDMGrams.quadgrams <- c(corpusTDMGrams.twitter.quadgrams,corpusTDMGrams.news.quadgrams,corpusTDMGrams.blogs.quadgrams)


  freqUnigram <- rowSums(as.matrix(corpusTDMGrams.unigrams))
  freqBigram <- rowSums(as.matrix(corpusTDMGrams.bigrams))
  freqTrigram <- rowSums(as.matrix(corpusTDMGrams.trigrams))
  freqQuadgram <- rowSums(as.matrix(corpusTDMGrams.quadgrams))

  if(saveGramsToFile) {
    write.table(freqBigram,file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\bigrams.csv")
    write.table(freqTrigram,file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\trigrams.csv")
    write.table(freQuadgram,file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\quadgrams.csv")
  }

  bigramSGT <- SimpleGT(table(freqBigram))
  trigramSGT <- SimpleGT(table(freqTrigram))
  quadgramSGT <- SimpleGT(table(freqQuadgram))

  wordFrameBigram <- buildWordFrameGram(freqBigram);
  wordFrameTrigram <- buildWordFrameGram(freqTrigram);
  wordFrameQuadgram <- buildWordFrameGram(freqQuadgram);
  wordFrameGrams <- list("wordFrameBigram"=wordFrameBigram,"wordFrameTrigram"=wordFrameTrigram,wordFrameQuadgram="wordFrameQuadgram")

  if(saveGramsToFile) {
    write.table(wordFrameBigram,file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\bigrams_wf.csv")
    write.table(wordFrameTrigram,file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\trigrams_wf.csv")
    write.table(wordFrameQuadgram,file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\quadgrams_wf.csv")
  }
  return(wordFrameGrams)
}


#wordFrameBigram[grep("would die",wordFrameBigram$word),]










