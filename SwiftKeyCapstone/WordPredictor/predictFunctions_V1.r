

#http://www.grsampson.net/D_SGT.c
#http://rstudio-pubs-static.s3.amazonaws.com/33754_4d463ac84bd24721bb9fe6a707ef6236.html
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
  #print(linearFit$coefficients)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  #plot(SGT_DT$logr, SGT_DT$logZ)
  #abline(linearFit,col="red")
  
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

buildWordFrameGram <- function(freqGram) {
    wordFrameGram <- data.frame(word=names(freqGram),count=freqGram,stringsAsFactors=FALSE)
    gramSplitList <- strsplit(wordFrameGram$word, ' (?=[^ ]+$)', perl=TRUE)
    wordFrameGram$start <- sapply(gramSplitList,FUN=function(x) x[1])
    wordFrameGram$end <- sapply(gramSplitList,FUN=function(x) x[2]) 
    return(wordFrameGram)
}

buildWordFrameGramFromSavedFile <- function(freqGram) {
    wordFrameGram <- data.frame(word=rownames(freqGram),count=freqGram$x,stringsAsFactors=FALSE)
    gramSplitList <- strsplit(wordFrameGram$word, ' (?=[^ ]+$)', perl=TRUE)
    wordFrameGram$start <- sapply(gramSplitList,FUN=function(x) x[1])
    wordFrameGram$end <- sapply(gramSplitList,FUN=function(x) x[2]) 
    return(wordFrameGram)
}


buildWordFrames <- function(freqGrams){
  freqBigram=freqGrams$freqBigram
  freqTrigram=freqGrams$freqTrigram
  freqQuadgram=freqGrams$freqQuadgram

  wordFrameBigram <- buildWordFrameGramFromSavedFile(freqBigram);
  wordFrameTrigram <- buildWordFrameGramFromSavedFile(freqTrigram);
  wordFrameQuadgram <- buildWordFrameGramFromSavedFile(freqQuadgram);
  wordFrameGrams <- list("wordFrameBigram"=wordFrameBigram,"wordFrameTrigram"=wordFrameTrigram,"wordFrameQuadgram"=wordFrameQuadgram)
  return(wordFrameGrams)
}

buildSGT <- function(freqGrams) {
    bigramSGT <- SimpleGT(table(freqGrams$freqBigram))
    trigramSGT <- SimpleGT(table(freqGrams$freqTrigram))
    quadgramSGT <- SimpleGT(table(freqGrams$freqQuadgram))
    SGT <- list("bigramSGT"=bigramSGT,"trigramSGT"=trigramSGT,"quadgramSGT"=quadgramSGT)
}


loadGramsFromFile <- function() {
  #freqBigram=read.table(file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\bigrams.csv")
  #freqTrigram=read.table(file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\trigrams.csv")
  #freqQuadgram=read.table(file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\quadgrams.csv")
  freqBigram=read.table(file="bigrams.csv")
  freqTrigram=read.table(file="trigrams.csv")
  freqQuadgram=read.table(file="quadgrams.csv")
  freqGrams <- list("freqBigram"=freqBigram,"freqTrigram"=freqTrigram,"freqQuadgram"=freqQuadgram)
  return(freqGrams)
}



#Load grams
loadWordFrameGramsFromFile <- function() {
  #wordFrameBigram=read.table(file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\bigrams_wf.csv")
  #wordFrameTrigram=read.table(file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\trigrams_wf.csv")
  #wordFrameQuadgram=read.table(file="C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments\\quadgrams_wf.csv")
  wordFrameBigram=read.table(file="bigrams_wf.csv")
  wordFrameTrigram=read.table(file="trigrams_wf.csv")
  wordFrameQuadgram=read.table(file="quadgrams_wf.csv")
  wordFrameGrams <- list("wordFrameBigram"=wordFrameBigram,"wordFrameTrigram"=wordFrameTrigram,"wordFrameQuadgram"=wordFrameQuadgram)
  return(wordFrameGrams)
}

getNextWords <- function(wordFrameGrams,SGT,testSentence, wordCount=4) {
  #print("In getNextWords")
  testSentence = trim(testSentence)
  if( nchar(testSentence) ==0  ){
    return(c("Start typing"))
  } else {
    #print(paste("test sentence [ ",testSentence,"]"))
  }
  lastChar = substr(testSentence, nchar(testSentence),nchar(testSentence))
  if ( lastChar == ".") {
    return(c("The", "It", "We", "But"))
  }
  if ( lastChar == ",") {
    return(c("but", "hence", "while", "therefore"))
  }
  testSentence <- cleanSentence(testSentence)
  #print(paste("clean sentence[ ",testSentence,"]"))
  wordFrameBigram = wordFrameGrams$wordFrameBigram
  wordFrameTrigram=wordFrameGrams$wordFrameTrigram
  wordFrameQuadgram=wordFrameGrams$wordFrameQuadgram
  bigramSGT = SGT$bigramSGT
  trigramSGT <- SGT$trigramSGT
  quadgramSGT <- SGT$quadgramSGT

  testSentenceList <- unlist(strsplit(testSentence," "))
  noOfWords <- length(testSentenceList)

  testSentenceTrigrams <- trim(paste(testSentenceList[noOfWords-2],testSentenceList[noOfWords-1],testSentenceList[noOfWords]))
  #print(paste("Predicting next word ",testSentenceTrigrams))
  quadgramSubset <- wordFrameQuadgram[wordFrameQuadgram$start == testSentenceTrigrams,]
  if(nrow(quadgramSubset) >0) {
    quadgramSubset$prob <- sapply(quadgramSubset$count,FUN=function(x) quadgramSGT$p[quadgramSGT$r==x]) 
    quadgramSubset$wtprob <-  quadgramSubset$prob * 0.5
  }
  #print(quadgramSubset)

  testSentenceBigrams <- trim(paste(testSentenceList[noOfWords-1],testSentenceList[noOfWords]))
  #print(paste("Predicting next word ",testSentenceBigrams))
  trigramSubset <- wordFrameTrigram[wordFrameTrigram$start == testSentenceBigrams,]
  if(nrow(trigramSubset) >0 ) {
    trigramSubset$prob <- sapply(trigramSubset$count,FUN=function(x) trigramSGT$p[trigramSGT$r==x]) 
    trigramSubset$wtprob <-  trigramSubset$prob * 0.3
  } 
  #print(trigramSubset)
  #print(paste("Predicting next word ",testSentenceList[noOfWords]))
  if (!(testSentenceList[noOfWords] %in%  myStopWords) || (nrow(trigramSubset) ==0 && nrow(quadgramSubset)==0)){   #skip bigram if it is a stop word
    bigramSubset <- wordFrameBigram[wordFrameBigram$start == testSentenceList[noOfWords],]
    if(nrow(bigramSubset) >0 ) {
      bigramSubset$prob <- sapply(bigramSubset$count,FUN=function(x) bigramSGT$p[bigramSGT$r==x]) 
      bigramSubset$wtprob <-  bigramSubset$prob * 0.2
      useBigramSubset  = TRUE
    } else {
      useBigramSubset =  FALSE
    }
  } else {
    #print("Skipping wordFrameBigram lookup as it is a stop word")
    bigramSubset = data.frame()
    useBigramSubset  = FALSE
  }
  #Give weights to each model 0.5(quad), 0.3(tri) 0.2(bigram)
  #http://see.stanford.edu/materials/ainlpcs224n/transcripts/NaturalLanguageProcessing-Lecture02.html
  mergedSubset <- ddply(merge(quadgramSubset, trigramSubset, all=TRUE), .(end), summarise, wtprob=sum(wtprob))
  if(useBigramSubset) {
    mergedSubset <- ddply(merge(mergedSubset, bigramSubset, all=TRUE), .(end), summarise, wtprob=sum(wtprob))
  } 
  #print(mergedSubset)
  if(nrow(mergedSubset)>0) {
    mergedSubset <- head(mergedSubset[order(mergedSubset$wtprob, decreasing=TRUE), ], wordCount)
    #print(paste("returning ",mergedSubset$end),sep = "")
    return(mergedSubset$end)
  } else {
    return(c("the", "a", "an", "of"))
  }
}

predictNextWord <- function(testSentence, validResultsList=None) {
  testSentenceList <- unlist(strsplit(testSentence," "))
  noOfWords <- length(testSentenceList)

  testSentenceTrigrams <- paste(testSentenceList[noOfWords-2],testSentenceList[noOfWords-1],testSentenceList[noOfWords])
  print(paste("Predicting next word ",testSentenceTrigrams))
  quadgramSubset <- wordFrameQuadgram[wordFrameQuadgram$start == testSentenceTrigrams,]
  if(nrow(quadgramSubset) >0) {
    quadgramSubset$prob <- sapply(quadgramSubset$count,FUN=function(x) quadgramSGT$p[quadgramSGT$r==x]) 
    if(missing(validResultsList)) {
      #find the best match
      resultDF <- quadgramSubset[which.max(quadgramSubset$prob),]
      return(resultDF)
    } else {
      quadgramSubset <- quadgramSubset[quadgramSubset$end %in% resultsList,]
      if(nrow(quadgramSubset) >0) {
	print(paste("Quad - Probable resultset - ",quadgramSubset$word,quadgramSubset$prob))
	resultDF <- quadgramSubset[which.max(quadgramSubset$prob),]
        return(resultDF)
      }
    }
  }

  testSentenceBigrams <- paste(testSentenceList[noOfWords-1],testSentenceList[noOfWords])
  print(paste("Predicting next word ",testSentenceBigrams))
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
	print(paste("Tri - Probable resultset - ",trigramSubset$word,trigramSubset$prob))
	resultDF <- trigramSubset[which.max(trigramSubset$prob),]
        return(resultDF)
      }
    }
  } 
  
  if(useBigramForPrediction) {
  print(paste("Predicting next word ",testSentenceList[noOfWords]))
  bigramSubset <- wordFrameBigram[wordFrameBigram$start == testSentenceList[noOfWords],]
  if(nrow(bigramSubset) >0 ) {
    bigramSubset$prob <- sapply(bigramSubset$count,FUN=function(x) bigramSGT$p[bigramSGT$r==x]) 
    if(missing(validResultsList)) {
      resultDF <- bigramSubset[which.max(bigramSubset$prob),]
      return(resultDF)
    } else {
      bigramSubset <- bigramSubset[bigramSubset$end %in% resultsList,]
      if(nrow(bigramSubset) >0) {
	print(paste("Bi - Probable resultset - ",bigramSubset$word,bigramSubset$prob))
	resultDF <- bigramSubset[which.max(bigramSubset$prob),]
        return(resultDF)
      }
    }
  }
  }
  return(NULL)
}


#http://entrenchant.blogspot.com/2013/06/english-word-clouds-in-r.html
fix_contractions <- function(doc) {
   # "won't" is a special case as it does not expand to "wo not"
   doc <- gsub("won't", "will not", doc)
   doc <- gsub("n't", " not", doc)
   doc <- gsub("'ll", " will", doc)
   doc <- gsub("'re", " are", doc)
   doc <- gsub("'ve", " have", doc)
   doc <- gsub("'m", " am", doc)
   # 's could be is or possessive: it has no expansion
   doc <- gsub("'s", "", doc) 
   return(doc)
}

expand_contractions <- function(x) {
  multigsub(tolower(contractions$contraction),tolower(contractions$expanded),x)
}

aggregate.plurals <- function (v) {
    aggr_fn <- function(v, singular, plural) {
       if (! is.na(v[plural])) {
           v[singular] <- v[singular] + v[plural]
           v <- v[-which(names(v) == plural)]
       }
       return(v)
    }
    for (n in names(v)) {
       n_pl <- paste(n, 's', sep='')
       v <- aggr_fn(v, n, n_pl)
       n_pl <- paste(n, 'es', sep='')
       v <- aggr_fn(v, n, n_pl)
     }
     return(v)
 }

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

cleanSentence <- function(testSentence) {
  testSentence <- sent_detect(testSentence)
  testSentence <- testSentence[length(testSentence)]
  testSentence <- tolower(testSentence)
  testSentence <- removeNumbers(testSentence)
  if(expandContractions) {
    testSentence <- expand_contractions(testSentence)
  }
  testSentence <- removePunctuation(testSentence, preserve_intra_word_dashes = TRUE)
  if(removeStopWords) {
    testSentence <- removeWords(testSentence,myStopWords)
  }
  if(doStemming) {
    testSentence <- stemDocument(testSentence)
  }
  testSentence <- stripWhitespace(testSentence)
  testSentence <- trim(testSentence)
  return(testSentence)
}

#def stringSearchColumn_DataFrame(df, colName, regex):
#    newdf = DataFrame()
#    for idx, record in df[colName].iteritems():
#        if re.search(regex, record):
#            newdf = concat([df[df[colName] == record], newdf], ignore_index=True)
#    return newdf
