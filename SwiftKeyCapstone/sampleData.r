setwd("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\data\\Coursera-SwiftKey")

set.seed(1000)
maxLinesPerIterator=10000
sampleRatio=.20
conTwitter <- file("corpus_complete\\en_US.twitter.txt", "r")
length <- length(dataTwitter <- readLines(conTwitter, n = maxLinesPerIterator, warn = FALSE))
while (length > 0) {
   sampleData1 <- sample(dataTwitter,length*sampleRatio)
   sampleData2 <- sample(dataTwitter,length*sampleRatio)
   sampleData3 <- sample(dataTwitter,length*sampleRatio)
   sampleData4 <- sample(dataTwitter,length*sampleRatio)
   write(sampleData1,"corpus_sample20_1/en_US.twitter_sample20_1.txt",append=TRUE)
   write(sampleData2,"corpus_sample20_2/en_US.twitter_sample20_2.txt",append=TRUE)
   write(sampleData3,"corpus_sample20_3/en_US.twitter_sample20_3.txt",append=TRUE)
   write(sampleData4,"corpus_sample20_4/en_US.twitter_sample20_4.txt",append=TRUE)
   length <- length(dataTwitter <- readLines(conTwitter, n = maxLinesPerIterator, warn = FALSE))
}
close(conTwitter)

conUSBlogs <- file("corpus_complete\\en_US.blogs.txt", "r")
length <- length(dataUSBlogs <- readLines(conUSBlogs, n = maxLinesPerIterator, warn = FALSE))
while (length> 0) {
  sampleData1 <- sample(dataUSBlogs,length*sampleRatio)
  sampleData2 <- sample(dataUSBlogs,length*sampleRatio)
  sampleData3 <- sample(dataUSBlogs,length*sampleRatio)
  sampleData4 <- sample(dataUSBlogs,length*sampleRatio)
  write(sampleData1,"corpus_sample20_1/en_US.blogs_sample20_1.txt",append=TRUE)
  write(sampleData2,"corpus_sample20_2/en_US.blogs_sample20_2.txt",append=TRUE)
  write(sampleData3,"corpus_sample20_3/en_US.blogs_sample20_3.txt",append=TRUE)
  write(sampleData4,"corpus_sample20_4/en_US.blogs_sample20_4.txt",append=TRUE)
  length <- length(dataUSBlogs <- readLines(conUSBlogs, n = maxLinesPerIterator, warn = FALSE))
}
close(conUSBlogs)
conUSNews <- file("corpus_complete\\en_US.news.txt", "r")
length<- length(dataUSNews <- readLines(conUSNews, n = maxLinesPerIterator, warn = FALSE)) 
while (length> 0) {
  sampleData1 <- sample(dataUSNews,length*sampleRatio)
  sampleData2 <- sample(dataUSNews,length*sampleRatio)
  sampleData3 <- sample(dataUSNews,length*sampleRatio)
  sampleData4 <- sample(dataUSNews,length*sampleRatio)
  write(sampleData1,"corpus_sample20_1/en_US.news.txt_sample20_1.txt",append=TRUE)
  write(sampleData2,"corpus_sample20_2/en_US.news.txt_sample20_2.txt",append=TRUE)
  write(sampleData3,"corpus_sample20_3/en_US.news.txt_sample20_3.txt",append=TRUE)
  write(sampleData4,"corpus_sample20_4/en_US.news.txt_sample20_4.txt",append=TRUE)
  length<- length(dataUSNews <- readLines(conUSNews, n = maxLinesPerIterator, warn = FALSE)) 
}
close(conUSNews)

