setwd("C:\\Users\\Sham\\Documents\\DataScience\\capstone\\Assignments")
library(shiny)
runApp("WordPredictor")
#http://shiny.rstudio.com/gallery/word-cloud.html
#http://stackoverflow.com/questions/21935715/reading-a-local-file-into-a-shinyapp-hosted-on-server
#As for the general algorithm used in prediction, the plan is to use a “backoff” strategy, whereby the prediction algorithm looks at the previous two words and then looks at the most frequent word that follows those two words in the corpus. If there is no instance of such a trigram, the algorithm would look at the previous one word and look for the most frequent bigram starting with that word. If there is also no such bigram, the algorithm would just predict the most common word in the corpus. This algorithm could be implemented with simple subsetting operations in R, and will hopefully not be too slow.

#http://meefen.github.io/blog/2013/02/26/demo-of-using-twitter-hashtag-analytics-package-to-analyze-tweets-from-lak13/
