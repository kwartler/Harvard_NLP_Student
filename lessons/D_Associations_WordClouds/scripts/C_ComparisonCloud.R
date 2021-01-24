#' Title: Comparison Cloud
#' Purpose: Given two corpora find disjoint words and visualize
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Dec 28 2020

# Set the working directory
setwd("~/Desktop/Harvard_NLP_Student/lessons/D_Associations_WordClouds/data")

# Options
options(scipen = 999)

# Libs
library(tm)
library(qdap)
library(wordcloud)

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
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay', 'beer')

# Read in multiple files as individuals
txtFiles <- list.files(pattern = 'beer|chardonnay|coffee')

for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

# Collapse all the text for each subject
chardonnay <- paste(chardonnay.csv$text, collapse = ' ')
beer       <- paste(beer.csv$text, collapse = ' ')
coffee     <- paste(coffee.csv$text, collapse = ' ')

# Build the 2 document corpus
drinks <- c(chardonnay, beer, coffee)

# Vector Corpus
drinks <- VCorpus(VectorSource(drinks))

# Clean up the data
drinks <- cleanCorpus(drinks, stops)

# Make TDM with a different control parameter; just for fun
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
drinkTDM  <- TermDocumentMatrix(drinks, control = ctrl)
drinkTDMm <- as.matrix(drinkTDM)
dim(drinkTDMm)

# Make sure order is correct!  We always combined chardonnay then beer, so has to match!!
filesNames <- gsub('.csv','',txtFiles)
filesNames
colnames(drinkTDMm) <- c(filesNames[2], filesNames[1], filesNames[3])

# Examine
head(drinkTDMm)

# Make comparison cloud
comparison.cloud(drinkTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('tomato', 'goldenrod', 'darkgreen'),
                 scale=c(3,0.1))

# End