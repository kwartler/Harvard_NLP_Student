#' Title: Text 2 Vector
#' Purpose: Explore text2Vec
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Feb 22 2021
#'

# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/H_text2Vec_documentClassification/data")

# Libs
library(text2vec)
library(fst)
library(tm)
library(data.table)

# Read
text <- read_fst('Airbnb-boston_only.fst')

# Organize
text <- data.table(reviewID = text$review_id, 
                   comments = text$comments, 
                   reviewScores = text$review_scores_rating)

### Basic processing
text$comments <- tolower(text$comments)
text$comments <- removeWords(text$comments, c(stopwords('SMART'), 'Boston'))
text$comments <- removePunctuation(text$comments)
text$comments <- stripWhitespace(text$comments)
text$comments <- removeNumbers(text$comments)

# Token making
iterMaker <- itoken(text$comments, 
                    progressbar = T)
textVocab       <- create_vocabulary(iterMaker)
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 5)
vectorizer <- vocab_vectorizer(prunedtextVocab)
tcm <- create_tcm(iterMaker, vectorizer, skip_grams_window = 5)
dim(tcm)
as.matrix(tcm[1:5,1:5])

# Glove Set up
glove <- GlobalVectors$new(rank = 50, x_max = 10) # 50 dimensions, max number of co-occurences

# Fit the model
# Iterate 10 times:
# Adagrad optimization of learning rate 
gloveFit <- glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
dim(gloveFit)

# The model produces two vector a "main" and "context"
#"Essentially they are the same since model is symmetric. 
#From our experience learning two sets of word vectors leads to higher quality embeddings."
wordVectors <- gloveFit + t(glove$components)

# What did we get?
head(wordVectors)

# In these reviews this is the meaning of walk
wordVectors['walk',,drop=F]

# Let's explore the reviews context for "good walk"
goodWalks <- wordVectors['walk',,drop=F] - 
  wordVectors['disappointed',,drop=F] +
  wordVectors['good',,drop=F]

# This is a numeric representation of a good walk free from disappointment in our reviews
goodWalks

# Now you need to find the terms that have the closest cosine similarity (same as spherical kmeans)
cosSim <- sim2(x = wordVectors, y = goodWalks)
head(sort(cosSim[,1], decreasing = TRUE), 10)

# End