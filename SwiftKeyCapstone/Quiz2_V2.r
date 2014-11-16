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

useBigramForPrediction=TRUE
doStemming=FALSE
doStemCompletion=FALSE
removeStopWords=FALSE
removeUnusedObjects=FALSE
buildSGT=FALSE
buildLookUp=FALSE


#http://www.grsampson.net/D_SGT.c
SimpleGT <- function(table_N){
  #Simple Good Turing Algorithm - Gale And Simpson
  #Good Turing Smoothing

  # table_U is a table of frequency of frequencies
  # The frequencies are stored as names of the list in the table structure
  # the values are the frequency of frequencies.
  # In Good Turing Smoothing, we are concerned with the frequency of frequencies
  # So, to extract the number of times that words of frequency n occur in the training set, we need to do:
  # table(freq_B)[[as.character(pairCount)]]
  # In a tables with a number of holes or non-contiguous sequence of frequency of words,
  # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
  # to do this, create a vector that is in the numerical form of the names of the table

  # create a data table
  # r is the frequencies of various trigrams
  #n is the frequency of frquencies
  SGT_DT <- data.frame(r=as.numeric(names(table_N)),n=as.vector(table_N),Z=vector("numeric",length(table_N)), 
                           logr=vector("numeric",length(table_N)),
                           logZ=vector("numeric",length(table_N)),
                           r_star=vector("numeric",length(table_N)),
                           p=vector("numeric",length(table_N)))
                          #p=vector("numeric",length(table_N)),key="r")

  str(SGT_DT)
  
  num_r <- nrow(SGT_DT)
  for (j in 1:num_r) {
      if(j==1) {r_i<-0} else {r_i <- SGT_DT$r[j-1]}
      if(j==num_r){r_k<-SGT_DT$r[j]} else {r_k <- SGT_DT$r[j+1]}
      SGT_DT$Z[j] <- 2*SGT_DT$n[j] / (r_k-r_i)
      #print(paste(r_i,j,r_k))
  }
  SGT_DT$logr <- log(SGT_DT$r)
  SGT_DT$logZ <- log(SGT_DT$Z)
  linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
  print(linearFit$coefficients)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  plot(SGT_DT$logr, SGT_DT$logZ)
  abline(linearFit,col="red")
  
  use_y = FALSE
  for (j in 1:(num_r-1)) {
    r_plus_1 <- SGT_DT$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
    s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
    y<-r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      SGT_DT$r_star[j] <- y
    } else { 
      n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
      if(length(n_r_plus_1) == 0 ) {
        SGT_DT$r_star[j] <- y
        use_y = TRUE
      } else {
        n_r <- SGT_DT$n[j]
        x<-(r_plus_1) * n_r_plus_1/n_r
      
        if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
          SGT_DT$r_star[j] <- x
        }else {
          SGT_DT$r_star[j] <- y
          use_y = TRUE
        }
      }
    }
    if(j==(num_r-1)) {
      SGT_DT$r_star[j+1] <- y
    }
        
  }
  N <- sum(SGT_DT$n * SGT_DT$r)
  Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
  Po <- SGT_DT$n[1] / N
  SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
  
  return(SGT_DT)
  
}


predictNextWord <- function(testSentence, validResultsList=None) {
  testSentenceList <- unlist(strsplit(testSentence," "))
  noOfWords <- length(testSentenceList)
  testSentenceBigrams <- paste(testSentenceList[noOfWords-1],testSentenceList[noOfWords])

  trigramSubset <- wordFrameTrigram[wordFrameTrigram$start == testSentenceBigrams,]
  if(nrow(trigramSubset) >0 ) {
    trigramSubset$prob <- sapply(trigramSubset$count,FUN=function(x) trigramSGT$p[trigramSGT$r==x]) 
    if(missing(validResultsList)) {
      #find the best match
      resultDF <- trigramSubset[which.max(trigramSubset$prob),]
      return(resultDF)
    } else {
      trigramSubset <- trigramSubset[trigramSubset$end %in% resultsList,]
      if(nrow(trigramSubset) >0) {
	print(paste("Probable resultset - ",trigramSubset$word))
	resultDF <- trigramSubset[which.max(trigramSubset$prob),]
        return(resultDF)
      }
    }
  } 
  
  if(useBigramForPrediction) {
  print(paste("Using bigrams to predict next word ", testSentenceList[noOfWords]))
  bigramSubset <- wordFrameBigram[wordFrameBigram$start == testSentenceList[noOfWords],]
  if(nrow(bigramSubset) >0 ) {
    bigramSubset$prob <- sapply(bigramSubset$count,FUN=function(x) bigramSGT$p[bigramSGT$r==x]) 
    if(missing(validResultsList)) {
      resultDF <- bigramSubset[which.max(bigramSubset$prob),]
      return(resultDF)
    } else {
      bigramSubset <- bigramSubset[bigramSubset$end %in% resultsList,]
      if(nrow(bigramSubset) >0) {
	print(paste("Probable resultset - ",bigramSubset$word))
	resultDF <- bigramSubset[which.max(bigramSubset$prob),]
        return(resultDF)
      }
    }
  }
  }
  return(NULL)
}


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

myStopWords <-  c(stopwords("english"))
#myStopWords <-  c(stopwords("english"),stoplist,'-','–','�','‘')
#myStopWords <-   c ( "and" , "for" , "in" , "is " , "it " , "not " , "the" , "to" )

#idx <- which(myStopWords == "of")  #case of beer
#myStopWords <- myStopWords[-idx]


cleanSentence <- function(testSentence) {
  if(removeStopWords) {
    testSentence <- removeWords(testSentence,myStopWords)
  }
  testSentence <- stripWhitespace(testSentence)
  testSentence <- tolower(testSentence)
  testSentence <- removeNumbers(testSentence)
  testSentence <- removePunctuation(testSentence, preserve_intra_word_dashes = TRUE)
  if(doStemming) {
    testSentence <- stemDocument(testSentence)
  }
  return(testSentence)
}

#cname <-file.path(".","corpus_sample1")
#corpusData <- PCorpus(DirSource(cname),readerControl=list(reader=readPlain),dbControl=list(useDB=TRUE,dbName="./corpusSample1",dbType="DB1"))

#cname <-file.path(".","corpus_sample2")
#corpusData <- PCorpus(DirSource(cname),readerControl=list(reader=readPlain),dbControl=list(useDB=TRUE,dbName="./corpusSample2",dbType="DB1"))

#cname <-file.path(".","corpus_t")
#cname <-file.path(".","corpus_complete")
cname <-file.path(".","corpus_sample4")
corpusData <- VCorpus(DirSource(cname),readerControl=list(reader=readPlain,language="english",encoding='ANSI'))
#corpusData <- VCorpus(DirSource(cname),readerControl=list(reader=readPlain,language="english",encoding='utf-8'))
#corpusData <- PCorpus(DirSource(cname),readerControl=list(reader=readPlain),dbControl=list(useDB=TRUE,dbName="./corpusSample3",dbType="DB1"))

#cname <-file.path(".","corpus_complete")
#corpusData <- PCorpus(DirSource(cname),readerControl=list(reader=readPlain),dbControl=list(useDB=TRUE,dbName="./corpusDBNoStemming",dbType="DB1"))
corpusData <- tm_map(corpusData, stripWhitespace)
corpusData <- tm_map(corpusData, content_transformer(tolower))
corpusData <- tm_map(corpusData, removeNumbers)
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
  #if(removeUnusedObjects) {
    #rm(corpusData.dict)
    #gc()
  #}
}
summary(corpusData)

#corpusData <- corpusData[1]
#twitterData <- tm_map(corpusData[1], removeWords, stopwords("english"))
#corpusDataDTM <- DocumentTermMatrix(corpusData)
#freq <- colSums(as.matrix(corpusDataDTM))
#ord <- order(freq)
#freq[tail(ord)]
#dtms <- removeSparseTerms(corpusDataDTM, 0.1)
#freq1 <- colSums(as.matrix(dtms))

#findFreqTerms(corpusDataDTM,lowfreq=1000)
#findFreqTerms(corpusDataDTM, lowfreq=100)

#findAssocs(twitterDataDTM, "love", corlimit=0.6)

#plot(corpusDataDTM, terms=findFreqTerms(corpusDataDTM, lowfreq=100)[1:50], corThreshold=0.5)
#dtm =twitterDataDTM
#freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

#library(ggplot2)
#subset(wf, freq>500) %>%
#ggplot(aes(word, freq)) +
#geom_bar(stat="identity") +
#theme(axis.text.x=element_text(angle=45, hjust=1))

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
corpusTDMUnigrams <- TermDocumentMatrix(corpusData, control = list(wordLengths=c(2,15),tokenize = UnigramTokenizer))
removeSparseTerms(corpusTDMUnigrams,.2)


if(useBigramForPrediction) {
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  corpusTDMBigrams <- TermDocumentMatrix(corpusData, control = list(wordLengths=c(2,45),tokenize = BigramTokenizer))
  removeSparseTerms(corpusTDMBigrams,.2)
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


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
corpusDTMTrigrams <- TermDocumentMatrix(corpusData, control = list(wordLengths=c(2,45), tokenize = TrigramTokenizer))
#corpusDTMTrigrams <- TermDocumentMatrix(corpusData, control = list(minWordLength=3, tokenize = TrigramTokenizer))
#corpusDTMTrigrams <- TermDocumentMatrix(corpusData, control = list(stemming = function(word) wordStem(word, language = "english"),tokenize = TrigramTokenizer))
#corpusDTMTrigrams <- TermDocumentMatrix(corpusData, control = list(tokenize = TrigramTokenizer))
removeSparseTerms(corpusDTMTrigrams,.2)
if(doStemCompletion) {
  corpusDTMTrigrams.stem <- stemCompletion(rownames(corpusDTMTrigrams), dictionary=corpusData.dict, type=c("prevalent"))
  # change to stem completed row names
  rownames(corpusDTMTrigram) <- as.vector(corpusDTMTrigrams.stem)
}



#save(corpusDTMTrigrams, file='corpusDTMTrigrams.rdata')

#m <- as.matrix(corpusDTMTrigrams)
#write.csv(m, file="dtm.csv")

freqTrigram <- rowSums(as.matrix(corpusDTMTrigrams))
if(buildSGT == TRUE) {
  trigramSGT <- SimpleGT(table(freqTrigram))
}

#N=length(freqTrigram)
#wordFrameTrigram <- data.frame(word=character(N), count=numeric(N), start=character(N), end=character(N),stringsAsFactors=FALSE)

#for (i in 1:N) { 
#  val <- freqTrigram[i]
#   word <- names(val)
#   wordFrameTrigram[i,]$word = word
#   wordFrameTrigram[i,]$count = val[[1]]
#   triList <- strsplit(word, ' (?=[^ ]+$)', perl=TRUE)
#   wordFrameTrigram[i,]$start = triList[1]
#   wordFrameTrigram[i,]$end = triList[2]
#}

wordFrameTrigram <- data.frame(word=names(freqTrigram),count=freqTrigram,stringsAsFactors=FALSE)
#trigramSplitList <- strsplit(wordFrameTrigram$word," ")
#if(removeUnusedObjects) {
  #rm(corpusDTMTrigrams)
  #gc()
#}
if(buildLookUp==TRUE) {
  trigramSplitList <- strsplit(wordFrameTrigram$word, ' (?=[^ ]+$)', perl=TRUE)
  wordFrameTrigram$start <- sapply(trigramSplitList,FUN=function(x) x[1])
  wordFrameTrigram$end <- sapply(trigramSplitList,FUN=function(x) x[2]) 
  #if(removeUnusedObjects) {
#   rm(trigramSplitList)
#   rm(corpusData)
#   gc()
# }
}







testSentence1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
testSentence1 <- cleanSentence(testSentence1)
resultsList <- c('soda','cheese','pretzels','beer')
resultDF <- predictNextWord(testSentence1, resultsList)
if(!is.null(resultDF) ){
   print(paste("Question 1 - ", resultDF$word))
} else {
   print("Question 1 result not found")
}
#beer
testSentence2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
testSentence2 <- cleanSentence(testSentence2)
resultsList <- c('world', 'universe','best','most')
resultDF <- predictNextWord(testSentence2, resultsList)
if(!is.null(resultDF)){
   print(paste("Question 2 - ", resultDF$word))
} else {
   print("Question 2 result not found")
}
#world
testSentence3 <- "Hey sunshine, can you follow me and make me the"
testSentence3 <- cleanSentence(testSentence3)
resultsList <- c('bluest','saddest', 'smelliest', 'happiest')
resultDF <- predictNextWord(testSentence3, resultsList)
if(!is.null(resultDF) ){
   print(paste("Question 3 - ", resultDF$word))
} else {
   print("Question 3 result not found")
}
#happiest
testSentence4 <- "Very early observations on the Bills game: Offense still struggling but the"
testSentence4 <- cleanSentence(testSentence4)
resultsList <- c('crowd', 'defense', 'referees', 'players') 
resultDF <- predictNextWord(testSentence4, resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 4 - ", resultDF$word))
} else {
   print("Question 4 result not found")
}
#crowd --> wrong --> players(1)  referee(3)  ---> referee and crowd are wrong
testSentence5 <- "Go on a romantic date at the"
testSentence5 <- cleanSentence(testSentence5)
resultsList <-c('grocery','beach','mall','movies') 
resultDF <- predictNextWord(testSentence5,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 5 - ", resultDF$word))
} else {
   print("Question 5 result not found")
}
#beach
testSentence6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
testSentence6 <- cleanSentence(testSentence6)
resultsList <-c('horse', 'way', 'motorcycle', 'phone')
resultDF <- predictNextWord(testSentence6,resultsList)
if(!is.null(resultDF)   ){
   print(paste("Question 6 - ", resultDF$word))
} else {
   print("Question 6 result not found")
}
#way
testSentence7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
testSentence7 <- cleanSentence(testSentence7)
resultsList <- c('years', 'thing' ,'time', 'weeks')
#resultsList <- c('year', 'thing' ,'time', 'week')
resultDF <- predictNextWord(testSentence7,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 7 - ", resultDF$word))
} else {
   print("Question 7 result not found")
}
#year --> wrong ---> time
testSentence8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
testSentence8 <- cleanSentence(testSentence8)
resultsList <-c('eyes','toes', 'fingers' ,'ears')
#resultsList <-c('eye','toe', 'finger' ,'ear')
resultDF <- predictNextWord(testSentence8,resultsList)
if(!is.null(resultDF)   ){
   print(paste("Question 8 - ", resultDF$word))
} else {
   print("Question 8 result not found")
}
# ear --> wrong --> eyes(1)  or fingers(3)
testSentence9 <- "Be grateful for the good times and keep the faith during the"
testSentence9 <- cleanSentence(testSentence9)
resultsList <-c('sad', 'bad', 'hard', 'worse')
resultDF <- predictNextWord(testSentence9,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 9 - ", resultDF$word))
} else {
   print("Question 9 result not found")
}
#hard --> wrong  --> bad (bigram)  hard sad worse  --> during the worst(3)     -> hard and worst are wrong
testSentence10 <- "If this isn't the cutest thing you've ever seen, then you must be"
testSentence10 <- cleanSentence(testSentence10)
resultsList <-c('asleep', 'callous', 'insensitive', 'insane')
resultDF <- predictNextWord(testSentence10,resultsList)
if(!is.null(resultDF)  ){
   print(paste("Question 10 - ", resultDF$word))
} else {
   print("Question 10 result not found")
}
#asleep(1) --> ... insane(1)    --wrong(7 not available choise)  - insane is correct

#library(data.table)
#dt <- as.data.table(as.data.frame(as.matrix(twitterDTM)), keep.rownames=TRUE)
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


container <- create_container(twitterDataDTM, USCongress$major, trainSize=1:4000, testSize=4001:4449, virgin=FALSE)


