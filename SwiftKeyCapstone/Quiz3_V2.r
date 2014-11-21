set.seed(1000)
rm(list=ls())
gc()
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

useBigramForPrediction=TRUE
doStemming=FALSE
doStemCompletion=FALSE
removeStopWords=FALSE
#removeContractions=FALSE
expandContractions=TRUE
removeUnusedObjects=FALSE
buildSGT=TRUE
buildLookUp=TRUE
saveGramsToFile=FALSE
loadGramsFromFile=TRUE
sparsePercentage=0.50


source("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\predictFunctions.r")

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

#myStopWords <-  c(stopwords("english"))
#myStopWords <-  c(stopwords("english"),stoplist,'-','â€“','â€','â€˜')
#myStopWords <-   c ( "and" , "for" , "in" , "is " , "it " , "not " , "the" , "to" )

#idx <- which(myStopWords == "of")  #case of beer
#myStopWords <- myStopWords[-idx]

#mycontractions <-   grep("'", stopwords(), value=TRUE)
#removeWords("i'd like a soda, please", grep("'", stopwords(), value=TRUE))


cname <-file.path(".","corpus_sample20_1")
corpusData <- VCorpus(DirSource(cname),readerControl=list(reader=readPlain,language="english",encoding='ANSI'))
corpusData <- tm_map(corpusData, stripWhitespace)
corpusData <- tm_map(corpusData, content_transformer(tolower))
corpusData <- tm_map(corpusData, removeNumbers)
if(expandContractions) {
   corpusData <- tm_map(corpusData, content_transformer(expand_contractions))
}
corpusData <- tm_map(corpusData, removePunctuation, preserve_intra_word_dashes = TRUE)
if(removeStopWords) {
  corpusData <- tm_map(corpusData, removeWords, myStopWords)
}

summary(corpusData)
#corpusData <- tm_map(corpusData, function(x) gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", x)) 
#stemming #http://www.rdatamining.com/examples/text-mining
if(doStemming) {
  corpusData.dict <- corpusData
  corpusData <- tm_map(corpusData, stemDocument,language="english")
  #corpusData <- tm_map(corpusData, stemCompletion, dictionary=corpusData.dict)
  if(removeUnusedObjects) {
    rm(corpusData.dict)
    gc()
  }
}
summary(corpusData)

#corpusData <- corpusData[1]
#twitterData <- tm_map(corpusData[1], removeWords, stopwords("english"))
#corpusDataTDM <- DocumentTermMatrix(corpusData)
#freq <- colSums(as.matrix(corpusDataTDM))
#ord <- order(freq)
#freq[tail(ord)]
#dtms <- removeSparseTerms(corpusDataTDM, 0.1)
#freq1 <- colSums(as.matrix(dtms))

#findFreqTerms(corpusDataTDM,lowfreq=1000)
#findFreqTerms(corpusDataTDM, lowfreq=100)

#findAssocs(twitterDataTDM, "love", corlimit=0.6)

#plot(corpusDataTDM, terms=findFreqTerms(corpusDataTDM, lowfreq=100)[1:50], corThreshold=0.5)
#dtm =twitterDataTDM
#freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

#library(ggplot2)
#subset(wf, freq>500) %>%
#ggplot(aes(word, freq)) +
#geom_bar(stat="identity") +
#theme(axis.text.x=element_text(angle=45, hjust=1))

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
minWordLength=2  #1*2 
maxWordLength=15  #1*15 
corpusTDMUnigrams <- TermDocumentMatrix(corpusData, control = list(wordLengths = c(minWordLength, maxWordLength), tokenize = UnigramTokenizer))
corpusTDMUnigrams <- removeSparseTerms(corpusTDMUnigrams,sparsePercentage)

if(useBigramForPrediction) {
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  minWordLength=5  #2*2 + 1 spaces
  maxWordLength=31  #2*15 +1 spaces
  corpusTDMBigrams <- TermDocumentMatrix(corpusData, control = list(wordLengths = c(minWordLength, maxWordLength), tokenize = BigramTokenizer))
  corpusTDMBigrams <- removeSparseTerms(corpusTDMBigrams,sparsePercentage)
  freqBigram <- rowSums(as.matrix(corpusTDMBigrams))
  if(buildSGT == TRUE) {
    bigramSGT <- SimpleGT(table(freqBigram))
  }
  wordFrameBigram <- data.frame(word=names(freqBigram),count=freqBigram,stringsAsFactors=FALSE)
  if(removeUnusedObjects) {
    rm(corpusTDMBigrams)
    gc()
  }
  if(buildLookUp==TRUE) {
    bigramSplitList <- strsplit(wordFrameBigram$word," ")
    wordFrameBigram$start <- sapply(bigramSplitList,FUN=function(x) x[1]) 
    wordFrameBigram$end <- sapply(bigramSplitList,FUN=function(x) x[2]) 
    if(removeUnusedObjects) {
       rm(bigramSplitList)
      gc()
    }
  }
}
#wordFrameBigram[grep("would die",wordFrameBigram$word),]

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
minWordLength=8  #3*2 + 2 spaces
maxWordLength=47  #3*15 +2 spaces
corpusTDMTrigrams <- TermDocumentMatrix(corpusData, control = list(wordLengths = c(minWordLength, maxWordLength), tokenize = TrigramTokenizer))
corpusTDMTrigrams <- removeSparseTerms(corpusTDMTrigrams,sparsePercentage)
if(doStemCompletion) {
  corpusTDMTrigrams.stem <- stemCompletion(rownames(corpusTDMTrigrams), dictionary=corpusData.dict, type=c("prevalent"))
  # change to stem completed row names
  rownames(corpusTDMTrigram) <- as.vector(corpusTDMTrigrams.stem)
}

freqTrigram <- rowSums(as.matrix(corpusTDMTrigrams))
if(buildSGT == TRUE) {
  trigramSGT <- SimpleGT(table(freqTrigram))
}

wordFrameTrigram <- data.frame(word=names(freqTrigram),count=freqTrigram,stringsAsFactors=FALSE)
if(buildLookUp==TRUE) {
  trigramSplitList <- strsplit(wordFrameTrigram$word, ' (?=[^ ]+$)', perl=TRUE)
  wordFrameTrigram$start <- sapply(trigramSplitList,FUN=function(x) x[1])
  wordFrameTrigram$end <- sapply(trigramSplitList,FUN=function(x) x[2]) 
  if(removeUnusedObjects) {
    rm(trigramSplitList)
    gc()
  }
}

QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
minWordLength=9  #4*2 + 1 spaces
maxWordLength=63  #4*15 +3 spaces
corpusTDMQuadgrams <- TermDocumentMatrix(corpusData, control = list(wordLengths = c(minWordLength, maxWordLength), tokenize = QuadgramTokenizer))
corpusTDMQuadgrams <- removeSparseTerms(corpusTDMQuadgrams,sparsePercentage)
if(doStemCompletion) {
  corpusTDMQuadgrams.stem <- stemCompletion(rownames(corpusTDMQuadgrams), dictionary=corpusData.dict, type=c("prevalent"))
  rownames(corpusTDMQuadgram) <- as.vector(corpusTDMQuadgrams.stem)
}

freqQuadgram <- rowSums(as.matrix(corpusTDMQuadgrams))
if(buildSGT == TRUE) {
  quadgramSGT <- SimpleGT(table(freqQuadgram))
}

wordFrameQuadgram <- data.frame(word=names(freqQuadgram),count=freqQuadgram,stringsAsFactors=FALSE)
if(buildLookUp==TRUE) {
  quadgramSplitList <- strsplit(wordFrameQuadgram$word, ' (?=[^ ]+$)', perl=TRUE)
  wordFrameQuadgram$start <- sapply(quadgramSplitList,FUN=function(x) x[1])
  wordFrameQuadgram$end <- sapply(quadgramSplitList,FUN=function(x) x[2]) 
  if(removeUnusedObjects) {
    rm(quadgramSplitList)
    gc()
  }
}

#saveGrams
if(saveGramsToFile) {
  UnigramsAsMatrix=as.matrix(corpusTDMUnigrams)
  write.table(UnigramsAsMatrix,file="unigrams.csv")

  BigramsAsMatrix=as.matrix(corpusTDMBigrams)
  write.table(BigramsAsMatrix,file="bigrams.csv")

  TrigramsAsMartrix=as.matrix(corpusTDMTrigrams)
  write.table(TrigramsAsMartrix,file="trigrams.csv")

  QuadgramsAsMatrix=as.matrix(corpusTDMQuadgrams)
  write.table(QuadgramsAsMatrix,file="quadgrams.csv")
}

#Load grams

if(loadGramsFromFile) {
  UnigramsFromFile=read.table(file="unigrams.csv")
  colnames(UnigramsFromFile)<-gsub("^[X](.*)","\\1",colnames(UnigramsFromFile))
  UnigramsAsWFM = as.wfm(UnigramsFromFile)
  corpusTDMUnigrams = as.tdm(UnigramsAsWFM)
  rm(UnigramsFromFile)
  rm(UnigramsAsWFM)

  BigramsFromFile=read.table(file="bigrams.csv")
  colnames(BigramsFromFile)<-gsub("^[X](.*)","\\1",colnames(BigramsFromFile))
  BigramsAsWFM = as.wfm(BigramsFromFile)
  corpusTDMBigrams = as.tdm(BigramsAsWFM)
  rm(BigramsFromFile)
  rm(BigramsAsWFM)

  TrigramsFromFile=read.table(file="trigrams.csv")
  colnames(TrigramsFromFile)<-gsub("^[X](.*)","\\1",colnames(TrigramsFromFile))
  TrigramsAsWFM = as.wfm(TrigramsFromFile)
  corpusTDMTrigrams = as.tdm(TrigramsAsWFM)
  rm(TrigramsFromFile)
  rm(TrigramsAsWFM)

  QuadgramsFromFile=read.table(file="quadgrams.csv")
  colnames(QuadgramsFromFile)<-gsub("^[X](.*)","\\1",colnames(QuadgramsFromFile))
  QuadgramsAsWFM = as.wfm(QuadgramsFromFile)
  corpusTDMQuadgrams = as.tdm(QuadgramsAsWFM)
  rm(QuadgramsFromFile)
  rm(QuadgramsAsWFM)

  buildSGT=TRUE
  buildLookUp=TRUE

  freqBigram <- rowSums(as.matrix(corpusTDMBigrams))
  #freqBigram <- sort(freqBigram[which(freqBigram >1)],decreasing=TRUE)
  wordFrameBigram <- data.frame(word=names(freqBigram),count=freqBigram,stringsAsFactors=FALSE)
  if(buildLookUp==TRUE) {
    bigramSplitList <- strsplit(wordFrameBigram$word," ")
    wordFrameBigram$start <- sapply(bigramSplitList,FUN=function(x) x[1]) 
    wordFrameBigram$end <- sapply(bigramSplitList,FUN=function(x) x[2]) 
  }

  freqTrigram <- rowSums(as.matrix(corpusTDMTrigrams))
  #freqTrigram <- sort(freqTrigram[which(freqTrigram >1)],decreasing=TRUE)
  wordFrameTrigram <- data.frame(word=names(freqTrigram),count=freqTrigram,stringsAsFactors=FALSE)
  if(buildLookUp==TRUE) {
    trigramSplitList <- strsplit(wordFrameTrigram$word, ' (?=[^ ]+$)', perl=TRUE)
    wordFrameTrigram$start <- sapply(trigramSplitList,FUN=function(x) x[1])
    wordFrameTrigram$end <- sapply(trigramSplitList,FUN=function(x) x[2]) 
  }
  if(buildSGT) {
    bigramSGT <- SimpleGT(table(freqBigram))
    trigramSGT <- SimpleGT(table(freqTrigram))
    quadgramSGT <- SimpleGT(table(freqQuadgram))
  }
}





options("scipen"=100, "digits"=4)

testSentence1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
testSentence1 <- cleanSentence(testSentence1)
resultsList <- c('give','die','sleep','eat')
resultDF <- predictNextWord(testSentence1, resultsList)
if(!is.null(resultDF) ){
   print(paste("Question 1 - ", resultDF$word))
} else {
   print("Question 1 result not found")
}
#[1] "Predicting next word  and i would"
#[1] "Predicting next word  i would"
#[1] "Tri - Probable resultset -  i would die 0.00000690477452243231"   
#[2] "Tri - Probable resultset -  i would eat 0.00000402037182913177"   
#[3] "Tri - Probable resultset -  i would give 0.0000191903832535801"   
#[4] "Tri - Probable resultset -  i would sleep 0.000000581119109276179"
# die --> correct


testSentence2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
testSentence2 <- cleanSentence(testSentence2)
resultsList <- c('financial', 'spiritual','marital','horticultural')
resultDF <- predictNextWord(testSentence2, resultsList)
if(!is.null(resultDF)){
   print(paste("Question 2 - ", resultDF$word))
} else {
   print("Question 2 result not found")
}
#[1] "Predicting next word  me about his"
#[1] "Predicting next word  about his"
#[1] "Predicting next word  his"
#[1] "Bi - Probable resultset -  his financial 0.000000904393147603526"
#financial --> wrong

testSentence3 <- "I'd give anything to see arctic monkeys this"
testSentence3 <- cleanSentence(testSentence3)
resultsList <- c('month','morning', 'weekend', 'decade')
resultDF <- predictNextWord(testSentence3, resultsList)
if(!is.null(resultDF) ){
   print(paste("Question 3 - ", resultDF$word))
} else {
   print("Question 3 result not found")
}
#[1] "Bi - Probable resultset -  this decade 0.00000192910982731086"
#[2] "Bi - Probable resultset -  this month 0.000059816544687725"   
#[3] "Bi - Probable resultset -  this morning 0.000217307008332408" 
#[4] "Bi - Probable resultset -  this weekend 0.00020764088937858" 
#weekend --> morning
# using "to see this"  --> weekend --> correct

testSentence4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
testSentence4 <- cleanSentence(testSentence4)
resultsList <- c('happiness', 'stress', 'sleepiness', 'hunger') 
resultDF <- predictNextWord(testSentence4, resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 4 - ", resultDF$word))
} else {
   print("Question 4 result not found")
}
[1] "Using bigrams to predict next word  your"
[1] "Probable resultset -  your happiness 0.00000154427985281499"
[2] "Probable resultset -  your hunger 0.0000000405824824102412" 
[3] "Probable resultset -  your stress 0.0000000405824824102412" 
#happiness --> wrong ---> stress??

testSentence5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
testSentence5 <- cleanSentence(testSentence5)
resultsList <-c('minute','picture','walk','look') 
resultDF <- predictNextWord(testSentence5,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 5 - ", resultDF$word))
} else {
   print("Question 5 result not found")
}
#[1] "Quad - Probable resultset -  to take a look 0.0000298387596717372"   
#[2] "Quad - Probable resultset -  to take a picture 0.0000315210632129198"
#[3] "Quad - Probable resultset -  to take a walk 0.00000720982573668943"
#look -->picture --> correct

testSentence6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
testSentence6 <- cleanSentence(testSentence6)
resultsList <-c('incident', 'matter', 'account', 'case')
resultDF <- predictNextWord(testSentence6,resultsList)
if(!is.null(resultDF)   ){
   print(paste("Question 6 - ", resultDF$word))
} else {
   print("Question 6 result not found")
}
#[1] "Probable resultset -  settle the case 0.0000000217493279925466"
#case --> wrong --> matter?

testSentence7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each "
testSentence7 <- cleanSentence(testSentence7)
resultsList <- c('hand', 'finger' ,'arm', 'toe')
resultDF <- predictNextWord(testSentence7,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 7 - ", resultDF$word))
} else {
   print("Question 7 result not found")
}
#[1] "Predicting next word  groceries in each"
#[1] "Predicting next word  in each"
#[1] "Tri - Probable resultset -  in each hand 0.000000581119109276179"
#hand --> correct

testSentence8 <- "Every inch of you is perfect from the bottom to the"
testSentence8 <- cleanSentence(testSentence8)
resultsList <-c('side','top', 'middle' ,'center')
resultDF <- predictNextWord(testSentence8,resultsList)
if(!is.null(resultDF)   ){
   print(paste("Question 8 - ", resultDF$word))
} else {
   print("Question 8 result not found")
}

[1] "Predicting next word  bottom to the"
[1] "Quad - Probable resultset -  bottom to the top 0.00000110361094601336"
#top --> correct

testSentence9 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
testSentence9 <- cleanSentence(testSentence9)
resultsList <-c('inside', 'weekly', 'outside', 'daily')
resultDF <- predictNextWord(testSentence9,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 9 - ", resultDF$word))
} else {
   print("Question 9 result not found")
}
#[1] "Bi - Probable resultset -  playing outside 0.00000128835799361751"
#outside --> correct

testSentence10 <- "I like how the same people are in almost all of Adam Sandler's"
testSentence10 <- cleanSentence(testSentence10)
resultsList <-c('movies', 'stories', 'novels', 'pictures')
resultDF <- predictNextWord(testSentence10,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 10 - ", resultDF$word))
} else {
   print("Question 10 result not found")
}
#
#[1] "Bi - Probable resultset -  of movies 0.00000374898209076261"  
#[2] "Bi - Probable resultset -  of novels 0.000000969004692391559" 
#[3] "Bi - Probable resultset -  of pictures 0.00000749977306300616"
#[4] "Bi - Probable resultset -  of stories 0.00000674948239001758" 
#movies --> correct

#library(data.table)
#dt <- as.data.table(as.data.frame(as.matrix(twitterTDM)), keep.rownames=TRUE)
#setkey(dt, rn)

#inspect(adtm) 
#
#findFreqTerms(adtm, lowfreq=10) # find terms with a frequency higher than 10
#findAssocs(adtm, "usa",.5) # just looking for some associations  
#findAssocs(adtm, "china",.5)

## Trigrams
#require(RWeka)
#TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
#tdm <- TermDocumentMatrix(a, control = list(tokenize = TrigramTokenizer))
#tdm <- removeSparseTerms(tdm, 0.75)
#inspect(tdm[1:5,1:5])


container <- create_container(twitterDataTDM, USCongress$major, trainSize=1:4000, testSize=4001:4449, virgin=FALSE)


colnames(m1)<-gsub("^[X](.*)","\\1",colnames(m1))
as.wfm(m1)

as.wfm()

 corp<-Corpus(DataframeSource(m1))

reut21578 <- system.file("texts", "crude", package = "tm")
corpus = VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
corpusTDMBigrams <- TermDocumentMatrix(corpus, control = list(wordLengths=c(2,45),tokenize = BigramTokenizer))
#minWordLength = 3
corpusTDMBigrams <- removeSparseTerms(corpusTDMBigrams,sparsePercentage)
freqBigram <- rowSums(as.matrix(corpusTDMBigrams))
#v <- sort(rowSums(as.matrix(td_mtx)), decreasing=TRUE)
m=as.matrix(corpusTDMBigrams)
write.table(m,file="dtm9.csv")
m1=read.table(file="dtm9.csv")
colnames(m1)<-gsub("^[X](.*)","\\1",colnames(m1))
m2 = as.wfm(m1)
m3 =as.tdm(m2)
object.size(corpusTDMBigrams)
object.size(m3)
head(inspect(corpusTDMBigrams))
head(inspect(m3))
m4=as.matrix(m3)
write.table(m4,file="dtm10.csv")
freqBigramM3 <- rowSums(as.matrix(m3))
