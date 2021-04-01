#' Title: A new way to make a DTM
#' Purpose: Build an elastic net for classification 
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Dec 28 2020
#'


# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/H_text2Vec_documentClassification/data")

# Libs
library(text2vec)
library(caret)
library(tm)

# Custom cleaning function
diagnosisClean<-function(xVec){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}

# Read
diabetes <- read.csv('diabetes_subset_8500.csv')

# Concantenate texts in 3 columns
diabetes$diagnosisText <- as.character(paste(diabetes$diag_1_desc,
                                             diabetes$diag_2_desc,
                                             diabetes$diag_3_desc, sep=' '))

### SAMPLE : Patritioning
idx              <- createDataPartition(diabetes$readmitted,p=.7,list=F)
trainDiabetesTxt <- diabetes[idx,]
testDiabetesTxt  <- diabetes[-idx,]

### EXPLORE
head(trainDiabetesTxt$diagnosisText,2)

table(trainDiabetesTxt$readmitted)

### MODIFY
trainDiabetesTxt$diagnosisText <- diagnosisClean(trainDiabetesTxt$diagnosisText)

# Initial iterator to make vocabulary
iterMaker <- itoken(trainDiabetesTxt$diagnosisText, 
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=stopwords('SMART'))
head(textVocab)
tail(textVocab)
nrow(textVocab)

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM 
diabetesDTM <- create_dtm(iterMaker, vectorizer)
dim(diabetesDTM)


# Now let's work with the test data which may have different terms or be missing training set terms
# Tokenize
testTokens   <- itoken(testDiabetesTxt$diagnosisText, 
                   tokenizer = word_tokenizer)

# Use the same vectorizer but with new iterator; no need to prune since the term reductions are inherited in the vectorizer function
testDTM <-create_dtm(testTokens,vectorizer)
dim(testDTM) #same number of terms!
