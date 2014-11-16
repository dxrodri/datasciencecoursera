setwd("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\data\\Coursera-SwiftKey")

conTwitter <- file("en_US.twitter.txt", "r")
#system('wc -l en_US.twitter.txt')
#2360148
maxLength<- 0
maxLine <- ''
maxLinesPerIterator = 10000
countLove <-0
countHate <-0
while (length(dataTwitter <- readLines(conTwitter, n = maxLinesPerIterator, warn = FALSE)) > 0) {
  maxLocation <- which.max(nchar(dataTwitter))
  maxLengthStep = nchar(dataTwitter[maxLocation])
  maxLineStep <- dataTwitter[maxLocation]
  #print(maxLengthStep)
  countLove <- countLove + length(grep('love[ .?]',dataTwitter))
  countHate <- countHate + length(grep('hate[ .?]',dataTwitter))
  biostatsLocation <- grep('biostats', dataTwitter)
  if(length(biostatsLocation) >0 ) {
   print(dataTwitter[biostatsLocation])
  }
  sentenceLocation <- grep('A computer once beat me at chess, but it was no match for me at kickboxing', dataTwitter)
  if(length(sentenceLocation) >0 ) {
   print(dataTwitter[sentenceLocation])
  }
  if(maxLengthStep > maxLength) {
     maxLength <- maxLengthStep
     maxLine <- maxLineStep
  } 
}
print(maxLine)
print(maxLength)
#213
print(countLove/countHate)
#4.8
close(conTwitter)

maxLength<- 0
maxLine <- ''
conUSBlogs <- file("en_US.blogs.txt", "r")
while (length(dataUSBlogs <- readLines(conUSBlogs, n = maxLinesPerIterator, warn = FALSE)) > 0) {
  maxLocation <- which.max(nchar(dataUSNews))
  maxLengthStep = nchar(dataUSBlogs[maxLocation])
  maxLineStep <- dataUSBlogs[maxLocation]
  #print(maxLengthStep)
  if(maxLengthStep > maxLength) {
     maxLength <- maxLengthStep
     maxLine <- maxLineStep
  } 
}
print(maxLine)
print(maxLength)
#40835
close(conUSBlogs)

maxLength<- 0
maxLine <- ''
conUSNews <- file("t.txt", "r")
while (length(dataUSNews <- readLines(conUSNews, n = maxLinesPerIterator, warn = FALSE)) > 0) {
  maxLocation <- which.max(nchar(dataUSNews))
  maxLengthStep = nchar(dataUSNews[maxLocation])
  maxLineStep <- dataUSNews[maxLocation]
  #print(maxLengthStep)
  if(maxLengthStep > maxLength) {
     maxLength <- maxLengthStep
     maxLine <- maxLineStep
  } 
}
print(maxLine)
print(maxLength)
#5760
close(conUSNews)

#==========================
#sample data
set.seed(1000)
maxLinesPerIterator=10000
sampleRatio=.30
conTwitter <- file("en_US.twitter.txt", "r")
length <- length(dataTwitter <- readLines(conTwitter, n = maxLinesPerIterator, warn = FALSE))
while (length > 0) {
   sampleData <- sample(dataTwitter,length*sampleRatio)
   write(sampleData,"corpus_sample4/en_US.twitter_sample4.txt",append=TRUE)
   length <- length(dataTwitter <- readLines(conTwitter, n = maxLinesPerIterator, warn = FALSE))
}
close(conTwitter)

conUSBlogs <- file("en_US.blogs.txt", "r")
length <- length(dataUSBlogs <- readLines(conUSBlogs, n = maxLinesPerIterator, warn = FALSE))
while (length> 0) {
  sampleData <- sample(dataUSBlogs,length*sampleRatio)
  write(sampleData,"corpus_sample4/en_US.blogs_sample4.txt",append=TRUE)
  length <- length(dataUSBlogs <- readLines(conUSBlogs, n = maxLinesPerIterator, warn = FALSE))
}
close(conUSBlogs)
conUSNews <- file("en_US.news.txt", "r")
length<- length(dataUSNews <- readLines(conUSNews, n = maxLinesPerIterator, warn = FALSE)) 
while (length> 0) {
  sampleData <- sample(dataUSNews,length*sampleRatio)
  write(sampleData,"corpus_sample4/en_US.news.txt_sample4.txt",append=TRUE)
  length<- length(dataUSNews <- readLines(conUSNews, n = maxLinesPerIterator, warn = FALSE)) 
}
close(conUSNews)


#==============================
readbig = function(file,samplesz,chunksz,nrec=0){
  if(nrec <= 0)nrec = length(count.fields(file))
  f = file(file,'r')
  on.exit(close(f))
  use = sort(sample(nrec,samplesz))
  now = readLines(f,1)
  k = length(strsplit(now,' +')[[1]])
  seek(f,0)
  result = matrix(0,samplesz,k)
  read = 0
  left = nrec
  got = 1
  while(left > 0){
    now = matrix(scan(f,n=chunksz*k),ncol=k,byrow=TRUE)
    begin = read + 1
    end = read + chunksz
    want = (begin:end)[begin:end %in% use] - read
    if(length(want) > 0){
      nowdat = now[want,]
      newgot = got + length(want) - 1
      result[got:newgot,] = nowdat
      got = newgot + 1
    }
    read = read + chunksz
    left = left - chunksz
  }
  return(result)
}


