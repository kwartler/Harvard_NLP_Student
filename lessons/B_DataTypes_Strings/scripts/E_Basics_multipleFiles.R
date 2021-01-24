#' Title: Text Organization for Bag of Words and some functions across multiple files
#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Dec 30 2020
#'

# Set the working directory
setwd("~/Desktop/Harvard_NLP_Student/lessons/B_DataTypes_Strings/data")

# Libs
library(tm)
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
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh')

# Dynamically get the file names
tmp <- list.files(path = "~/Desktop/Harvard_NLP_Student/lessons/B_DataTypes_Strings/data/clinton", 
                  pattern = '*.txt', 
                  full.names = T)

rawEmails <-pblapply(tmp, readLines)
rawEmails[[1]]

# Change to a single doc instead of a vector for each line; may not be needed in all cases
allEmails <-pblapply(rawEmails, paste, collapse = ' ')
allEmails[[1]]

# Lets add another specific processing step
for(i in 1:length(allEmails)){
  print(i)
  x <- allEmails[[i]]
  x <- gsub('UNCLASSIFIED U.S. Department of State Case No.|RELEASE IN FULL', '', x)
  x <- VCorpus(VectorSource(x))
  x <- cleanCorpus(x, stops)
  x <- content(x[[1]])
  allEmails[[i]] <- x
}
allEmails[[1]]

# If your list is just text you can unlist it
allEmails <- unlist(allEmails)
# if it is a list of data frames with the same columns you can make a single df
#allEmails <- do.call(rbind, allEmails)

#Another option is to provide a Directory Source; which is automatic but may require iteration to get your preprocessing correct
allEmails <- VCorpus(DirSource('~/Desktop/Harvard_NLP_Student/lessons/B_DataTypes_Strings/data/clinton/'))

# Clean 
allEmails <-cleanCorpus(allEmails, stops)

# Extract the text
df <- data.frame(text = unlist(sapply(allEmails, `[`, "content")),
                 stringsAsFactors=F)
head(df)

# Now collapse by rowname: C05758905.txt.content1 C05758905.txt.content2
df$doc_id<- unlist(lapply(strsplit(rownames(df), '\\.'), head, 1))
df <- aggregate(df$text, list(df$doc_id), paste, collapse=" ")
head(df) #then apply the gsub and other preprocessing as needed 

# End