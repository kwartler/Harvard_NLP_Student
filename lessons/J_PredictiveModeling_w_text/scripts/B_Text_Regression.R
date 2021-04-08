#' Title: Documents for Predictive Modeling
#' Purpose: Document regression example
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Apr 7, 2021
#'
# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/J_PredictiveModeling_ModelingEthicsBias/data")

# Libs
library(text2vec)
library(MLmetrics)
library(tm)
library(glmnet)
library(pROC)
library(caret)
library(tidyr)
library(ggplot2)
library(ggthemes)

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

summary(trainDiabetesTxt$number.diagnoses)

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

### MODEL(s)
#train text only model
textFit <- cv.glmnet(diabetesDTM,
                     y=trainDiabetesTxt$number.diagnoses,
                     alpha=1,
                     family='gaussian',
                     type.measure='mse',
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
title("GLMNET MSE Y")

# Predictions
textPreds   <- predict(textFit,
                      diabetesDTM,
                      s    = textFit$lambda.min)
summary(textPreds)
RMSE(textPreds,trainDiabetesTxt$number.diagnoses)
MAE(textPreds,trainDiabetesTxt$number.diagnoses)

# Organize
df <- data.frame(preds = textPreds[,1], 
                 actual = trainDiabetesTxt$number.diagnoses )
head(df)
df <- gather(df)
head(df)

# Visualize
ggplot(df, aes(x = key, y = value, fill = key)) +
  geom_boxplot() + 
  ggtitle('number.diagnoses pred vs actual') +
  theme_hc()

# Let's score the test set for evaluation
testDiabetesTxt$diagnosisText <- diagnosisClean(testDiabetesTxt$diagnosisText)
testTokens   <- itoken(testDiabetesTxt$diagnosisText, 
                       tokenizer = word_tokenizer)

# Use the same vectorizer but with new iterator
testDTM <-create_dtm(testTokens,vectorizer)
dim(testDTM)

testPreds <- predict(textFit, 
                     testDTM,
                     s    = textFit$lambda.min)
summary(testPreds)
RMSE(testPreds,testDiabetesTxt$number.diagnoses)
MAE(testPreds,testDiabetesTxt$number.diagnoses)
MAPE(testPreds,testDiabetesTxt$number.diagnoses)

df <- data.frame(preds = testPreds[,1], 
                 actual = testDiabetesTxt$number.diagnoses )
head(df)
tidyDF <- gather(df)
head(tidyDF)

# Visualize
ggplot(tidyDF, aes(x = key, y = value, fill = key)) +
  geom_boxplot() + 
  ggtitle('TEST set number.diagnoses pred vs actual') +
  theme_hc()
# End