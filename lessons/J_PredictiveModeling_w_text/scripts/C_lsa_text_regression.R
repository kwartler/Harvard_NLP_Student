#' Title: Documents for Predictive Modeling
#' Purpose: Document lsa example
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Apr 7, 2021
#'
# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/J_PredictiveModeling_w_text/data")

# Libs
library(fst)
library(lsa)
library(text2vec)
library(ranger)
library(ggplot2)
library(ggthemes)
library(MLmetrics)
library(tidyr)

# Data
airbnb <- read_fst('Airbnb-boston_only.fst')

# Organize
airbnb <- data.frame(reviewID = airbnb$review_id, 
                     comments = airbnb$comments, 
                     reviewScores = airbnb$review_scores_rating)
# Some review scores are missing so need to remove
airbnb <- airbnb[complete.cases(airbnb),]

# another method for partitioning; later we add a partition column
# Advantage: you can save the object with the same paritions for refitting later
idx <- sample(0:1,nrow(airbnb), replace = T, prob=c(1,4))

### Basic processing
airbnb$comments <- tolower(airbnb$comments)
airbnb$comments <- removeWords(airbnb$comments, c(stopwords('SMART'), 'Boston'))
airbnb$comments <- removePunctuation(airbnb$comments)
airbnb$comments <- stripWhitespace(airbnb$comments)
airbnb$comments <- removeNumbers(airbnb$comments)

# tokenize w text2vec
iterMaker <- itoken(airbnb$comments)
textVocab <- create_vocabulary(iterMaker)

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Finally, make a DTM 
txtDTM <- create_dtm(iterMaker, vectorizer)
dim(txtDTM)

# LSA Needs a TDM, it works on columns, so you have to transpose it!!!!! Takes a few min
#lsaTDM <- lsa(t(as.matrix(txtDTM)), 100)
#saveRDS(lsaTDM, 'lsaTDM.rds')
lsaTDM <- readRDS('lsaTDM.rds')

# Get the modeling part out
modelingVectors <- as.data.frame(lsaTDM$dk)
head(modelingVectors)

# Append Y & partition column
modelingVectors$reviewScores <- airbnb$reviewScores
modelingVectors$samplePartition <- idx

training <- subset(modelingVectors, 
                   modelingVectors$samplePartition==1)
test <- subset(modelingVectors, 
               modelingVectors$samplePartition==0)

# Fit a random forest model with Caret
form <- formula(paste('reviewScores ~', 
                      paste(names(training)[1:100], collapse = '+')))

# Model; 45 seconds, accepting defaults
# advanced parameter search https://uc-r.github.io/random_forests
st <- Sys.time()
fit <- ranger(form, data = training, importance = 'impurity')
Sys.time() - st

# model diagnostics
fit
fit$prediction.error
fit$variable.importance #not helpful :(

varImp <- sort(fit$variable.importance, decreasing = T)
varImp <- data.frame(var = names(varImp), importance = varImp)
varImp$var <- factor(varImp$var, levels = varImp$var)
ggplot(varImp[1:25,], aes(var, importance)) +
geom_col() +
coord_flip() +
ggtitle("Top 25 important variables") + theme_hc()

# Get predictions on the partitions
trainPreds <- predict(fit, training)
testPreds  <- predict(fit,  test)

# Assess
summary(trainPreds$predictions)
RMSE(trainPreds$predictions,training$reviewScores)
MAE(trainPreds$predictions,training$reviewScores)
MAPE(trainPreds$predictions,training$reviewScores)

summary(testPreds$predictions)
RMSE(testPreds$predictions,test$reviewScores)
MAE(testPreds$predictions,test$reviewScores)
MAPE(testPreds$predictions,test$reviewScores)

# Organize for the test data
df <- data.frame(preds  = testPreds$predictions, 
                 actual = test$reviewScores)
head(df)
df <- gather(df)
head(df)

# Visualize
ggplot(df, aes(x = key, y = value, fill = key)) +
  geom_boxplot() + 
  ggtitle('number.diagnoses pred vs actual') +
  theme_hc()

# End