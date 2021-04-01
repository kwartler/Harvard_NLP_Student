#' Title: ElasticNet with Vectorizer
#' Purpose: Create a penalized binary model
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Feb 22 2021
#'

# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/I_DocumentClassification_PredictiveModeling/data")

# Libs
library(text2vec)
library(MLmetrics)
library(tm)
library(glmnet)
library(pROC)

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
# 
trainDiabetesTxt$diagnosisText <- diagnosisClean(trainDiabetesTxt$diagnosisText)

# Initial iterator to make vocabulary
iterMaker <- itoken(trainDiabetesTxt$diagnosisText, 
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=stopwords('SMART'))

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM 
diabetesDTM <- create_dtm(iterMaker, vectorizer)
dim(diabetesDTM)

# Default is TF but if you want TF-IDF
#idf         <- get_idf(diabetesDTM)
#diabetesDTM <- transform_tfidf(diabetesDTM,idf)

### MODEL(s)
#train text only model
textFit <- cv.glmnet(diabetesDTM,
                     y=as.factor(trainDiabetesTxt$readmitted),
                     alpha=0.9,
                     family='binomial',
                     type.measure='auc',
                     nfolds=5,
                     intercept=F)


# Examine
head(coefficients(textFit),10)
# Subset to impacting terms
bestTerms <- subset(as.matrix(coefficients(textFit)), 
                    as.matrix(coefficients(textFit)) !=0)
bestTerms <- data.frame(term= rownames(bestTerms), value = bestTerms[,1])
head(bestTerms[order(bestTerms$value, decreasing=T), ])
tail(bestTerms[order(bestTerms$value, decreasing=T), ])
nrow(bestTerms)
ncol(diabetesDTM)

### ANALYZE
plot(textFit)
title("GLMNET Only Text")

# AUC
textPreds   <-as.logical(predict(textFit,
                                 diabetesDTM,
                                 type = 'class',
                                 s    = textFit$lambda.min))
textROC     <- roc((trainDiabetesTxt$readmitted*1), textPreds*1)
plot(textROC,col="blue",main="BLUE = Text Model Lift",adj=0)

# Let's score the test set for evaluation
testDiabetesTxt$diagnosisText <- diagnosisClean(testDiabetesTxt$diagnosisText)
testTokens   <- itoken(testDiabetesTxt$diagnosisText, 
                       tokenizer = word_tokenizer)

# Use the same vectorizer but with new iterator
testDTM <-create_dtm(testTokens,vectorizer)
dim(testDTM)

testPreds <- as.logical(predict(textFit, testDTM, 
                     type = 'class',
                     s    = textFit$lambda.min))
table(testPreds)
Accuracy(testPreds, testDiabetesTxt$readmitted)
table(testPreds, testDiabetesTxt$readmitted)

# End