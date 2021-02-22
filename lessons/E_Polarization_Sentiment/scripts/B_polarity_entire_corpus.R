#' Title: Polarity on a corpus
#' Purpose: Learn and calculate polarity 
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Feb 20, 2021
#'

# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/E_Polarization_Sentiment/data")

# Libs
library(tm)
library(qdap)

# Data I
text <- readLines('pharrell_williams_happy.txt')

# Polarity on the document
polarity(text)

# Does it Matter if we process it?
source('~/Desktop/Harvard_NLP_Student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')
txt <- VCorpus(VectorSource(text))
txt <- cleanCorpus(txt, stopwords("SMART"))
polarity(txt)

# Examine the polarity obj more
pol <- polarity(txt)

# Word count detail
pol$all$wc

# Polarity Detail
pol$all$polarity

# Pos Words ID'ed
pol$all$pos.words

# Neg Words ID'ed
pol$all$neg.words

# What are the doc words after polarity processing?
pol$all$text.var[[1]]
pol$all$text.var[[2]]
pol$all$text.var[[3]]

# Document View
pol$group

# End