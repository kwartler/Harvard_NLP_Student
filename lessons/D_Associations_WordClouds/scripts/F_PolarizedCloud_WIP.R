#' Title: Retailers Revised
#' Purpose: Build a tag cloud Experimental
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Dec 28 2020
#'


# libs
library(ggplot2)
library(tm)
library(dplyr)

# Set wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/D_Associations_WordClouds/data")

# Options 
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setlocale('LC_ALL','C')

# Bring in our supporting functions
source('~/Desktop/Harvard_NLP_Student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# Create custom stop words
stops <- c(stopwords('english'), 'carrefour', 'tesco')

# Bring in the two corpora; works with no meta data
retailers <- Corpus(DirSource("polarizedCloud/"))

# Get word counts
# mind the order is the same as is list.files('~/Desktop/hult_NLP_student/lessons/class3/data/polarizedCloud')
carreCount <- length(unlist(strsplit(content(retailers[[1]]), " ")))
tescoCount <- length(unlist(strsplit(content(retailers[[2]]), " ")))

# Clean & TDM
cleanRetail <- cleanCorpus(retailers, stops)
cleanRetail <- TermDocumentMatrix(cleanRetail)

# Create data frame from TDM
retailDF <- as.data.frame(as.matrix(cleanRetail))
head(retailDF)

# subset and calc diff
retailDF      <- subset(retailDF, retailDF[,1]>0 & retailDF[,2]>0) #in case stops make empty docs
retailDF$diff <- retailDF[,1]-retailDF[,2]

# Words used more by Carrefour
carrefourDF <- subset(retailDF, retailDF$diff > 0) # Said more  by carre
# Words used more by Tesco
tescoDF     <- subset(retailDF, retailDF$diff < 0) # Said more by tesco

# Calc how the much the term contributes to the specific corpus 
carrefourDF$density <- carrefourDF$carrefour.csv/carreCount
tescoDF$density     <- tescoDF$carrefour.csv/tescoCount

### Create visualization
topNum <- 20
dfA <- head(carrefourDF[order(carrefourDF$diff, decreasing = T),],topNum)
dfB<- head(tescoDF[order(abs(tescoDF$diff), decreasing = T),],topNum)


## Examine
dfA

ggplot(dfA, aes(x=diff, y=density))+
  geom_text(aes(size=dfA[,1], 
                label=row.names(dfA), colour=diff),
            hjust = "inward", vjust = "inward")+
  geom_text(data=dfB,aes(size=dfB[,1], 
                         label=row.names(dfA), colour=diff),
            hjust = "inward", vjust = "inward") +
  scale_size(range=c(3,11), name="Word Frequency")+scale_colour_gradient(low="darkred", high="darkblue", guide="none")+
  scale_x_continuous(breaks=c(min(dfB$diff),0,max(dfA$diff)),labels=c("Said More with Tesco","Said Equally","Said More with Carrefour"))+
  scale_y_continuous(breaks=c(0),labels=c(""))+xlab("")+ylab("")+theme_bw() 
# End