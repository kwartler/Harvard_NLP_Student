#' Title: Text Organization for Bag of Words
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: edward.kwartler@hult.edu
#' License: GPL>=3
#' Date: Dec 30 2020
#'

# Set the working directory
setwd("~/Desktop/hult_NLP_student/lessons/class3/data")

# Libs
library(tm)

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
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh')

# Data
text <- read.csv('coffee.csv', header=TRUE)

# As of tm version 0.7-3 tabular was deprecated
names(text)[1] <- 'doc_id' 

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Check Meta Data; brackets matter!!
txtCorpus[[4]]
meta(txtCorpus[4])
content(txtCorpus[[4]])

# Need a plain text cleaned copy?
df <- data.frame(text = unlist(sapply(txtCorpus, `[`, "content")),
                 stringsAsFactors=F)
#write.csv(df,'plain_coffee.csv',row.names = F)

# Or
df2 <- lapply(txtCorpus, content)

# Compare a single tweet
text$text[4]
df[4,]
df2[[4]] #still a list if needed

# Make a Document Term Matrix 
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)

# Examine DTM
txtDtmM[610:611,491:493]

# Get the most frequent terms
topTermsA <- colSums(txtDtmM)

# Add the terms
topTermsA <- data.frame(terms = colnames(txtDtmM), freq = topTermsA)

# Remove row attributes
rownames(topTermsA) <- NULL

######## OR Term Document Matrix depending on analysis
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtTdmM <- as.matrix(txtTdm)

# Examine TDM
txtTdmM[491:493,610:611]

# Get the most frequent terms
topTermsB <- rowSums(txtTdmM)

# Add the terms
topTermsB <- data.frame(terms = rownames(txtTdmM), freq = topTermsB)

# Remove row attributes
rownames(topTermsB) <- NULL

# Review, THEY ARE THE SAME!
head(topTermsA)
head(topTermsB)

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]
head(exampleReOrder)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# End