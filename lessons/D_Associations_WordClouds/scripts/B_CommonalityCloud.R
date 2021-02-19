#' Title: Commonality Cloud
#' Purpose: Given two corpora find words in common and visualize
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Jan 23 2021
#'

# Set the working directory
setwd("~/Desktop/Harvard_NLP_Student/lessons/D_Associations_WordClouds/data")

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)
library(pbapply)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay', 'coffee')

# Read in multiple files as individuals
txtFiles <- list.files(pattern = 'chardonnay|coffee')

for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

# Collapse all the text for each subject
chardonnay <- paste(chardonnay.csv$text, collapse = ' ')
coffee <- paste(coffee.csv$text, collapse = ' ')

# Build the 2 document corpus
drinks <- c(chardonnay, coffee)

# Vector Corpus
drinks <- VCorpus(VectorSource(drinks))

# Clean up the data
drinks <- cleanCorpus(drinks, stops)

# Make TDM
drinkTDM  <- TermDocumentMatrix(drinks)
drinkTDMm <- as.matrix(drinkTDM)
dim(drinkTDMm)

# Make sure order is correct!  We always combined chardonnay first, so has to match for other analysis
filesNames <- gsub('.csv','',txtFiles)
filesNames
colnames(drinkTDMm) <- c(filesNames[1], filesNames[2])

# Examine
head(drinkTDMm)

set.seed(123)
commonality.cloud(drinkTDMm, 
                  max.words=150, 
                  random.order=FALSE,
                  colors='blue',
                  scale=c(3.5,0.25))


# End