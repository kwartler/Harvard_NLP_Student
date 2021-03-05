#' Title: Named Entity Reco Syntactic Parsing Exploring Sentiment
#' Purpose: Apply openNLP
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Mar 3, 2021
#'

# WD
setwd("~/Desktop/Harvard_NLP_Student/lessons/F_openNLP_lemmatization/data")

# Inputs
fileFolder <- "~/Desktop/Harvard_NLP_Student/lessons/F_openNLP_lemmatization/data/trump"
testing    <- T

# Libs
library(pbapply)
library(stringr)
library(tm)
library(openNLP)
#install.packages("openNLPmodels.en", dependencies=TRUE, repos = "http://datacube.wu.ac.at/")
library(openNLPmodels.en)
library(qdap)
library(dplyr)

# Custom Functions needed bc of new class obj
txtClean <- function(x) {
  x <- x[,2] 
  x <- paste(x,collapse = " ")
  x <- replace_contraction(x)
  x <- removeNumbers(x)
  x <- as.String(x)
  return(x)
}

# Get data & Organize
tmp        <- list.files(path = fileFolder, pattern = '.csv', full.names = T)
txt        <- pblapply(tmp, read.csv)
names(txt) <- gsub('.csv', '', list.files(path = fileFolder, pattern = '.txt'))

if(testing == T){
  txt <- txt[1:25]
}

# Apply cleaning to all emails; review one email in the list
allTxt <- pblapply(txt, txtClean)

# POS Tagging
persons            <- Maxent_Entity_Annotator(kind='person')
locations          <- Maxent_Entity_Annotator(kind='location')
organizations      <- Maxent_Entity_Annotator(kind='organization')
sentTokenAnnotator <- Maxent_Sent_Token_Annotator(language='en')
wordTokenAnnotator <- Maxent_Word_Token_Annotator(language='en')
posTagAnnotator    <- Maxent_POS_Tag_Annotator(language='en')

# Annotate each document in  a loop
annotationsData <- list()
for (i in 1:length(allTxt)){
  print(paste('starting annotations on doc', i))
  annotations <- annotate(allTxt[[i]], 
                          list(sentTokenAnnotator, 
                               wordTokenAnnotator, 
                               posTagAnnotator, 
                               persons, 
                               locations, 
                               organizations))
  annDF           <- as.data.frame(annotations)[,2:5]
  annDF$features  <- unlist(as.character(annDF$features))
  
  
  annotationsData[[tmp[i]]] <- annDF
  print(paste('finished annotations on doc', i))
}

# Now obtain terms by index from each document using a NESTED loop 
allData<- list()
for (i in 1:length(allTxt)){
  x <- allTxt[[i]]       # get an individual document
  y <- annotationsData[[i]] # get an individual doc's annotation information
  print(paste('starting document:',i, 'of', length(allTxt)))
  
  # for each row in the annotation information, extract the term by index
  POSls <- list()
  for(j in 1:nrow(y)){
    annoChars <- ((substr(x,y[j,2],y[j,3]))) #substring position
    
    # Organize information in data frame
    z <- data.frame(doc_id = i,
                    type     = y[j,1],
                    start    = y[j,2],
                    end      = y[j,3],
                    features = y[j,4],
                    text     = as.character(annoChars))
    POSls[[j]] <- z
  }
  
  # Bind each documents annotations & terms from loop into a single DF
  docPOS       <- do.call(rbind, POSls)
  
  # So each document will have an individual DF of terms, and annotations as a list element
  allData[[i]] <- docPOS
}


###

POSdf <- do.call(rbind, allData)
locs <- subset(POSdf, 
               grepl("*location", POSdf$features) == T)

# Get polarity for the docs
# It may make sense not collapse these paragraphs in txtClean to get polarity for passges closer to each location
rawTxt <- pblapply(txt, "[" , 'transcription')
for(i in 1:length(rawTxt)){
  rawTxt[[i]]$doc_id <- i
  rawTxt[[i]]$section <- 1:nrow((rawTxt[[i]]))
}
rawTxt <- do.call(rbind, rawTxt)
#polTxt <- polarity(rawTxt$transcription, rawTxt$section) #document level
polTxt <- readRDS('polTxt.rds')


# ID the top N locations
topLocN <- 10
allLocs <- as.matrix(table(locs$text))
allLocs <- data.frame(locs = rownames(allLocs), 
                      ct = allLocs)
allLocs <- allLocs[order(allLocs$ct, decreasing = T), ]
rownames(allLocs) <- NULL
head(allLocs, topLocN)

# With these, let's get the polarity for sections that mention them
chk <- allLocs$locs[1:topLocN]
chkLst <- list()
for(i in 1:length(chk)){
  x <- grep(chk[i], rawTxt$transcription)
  chkLst[[make.names(chk[i])]] <- x
}

# Examine
chkLst$China

# Now let's grab the polarity for each subsection
polTxtSubs <- polTxt$all #individual section polarity

plotDF <- list()
for(i in 1:length(chkLst)){
  x <- polTxtSubs[chkLst[[i]],1:3]
  x <- x %>% group_by(section) %>% 
    mutate(prop = prop.table(wc))
  y <- weighted.mean(x$polarity,x$prop)
  wc <- aggregate(wc~section, x, sum)
  plotDF[[chk[i]]] <- data.frame(loc = names(chkLst[i]),
                                  locPol = y, locWC = sum(wc))
  
}
plotDF <- do.call(rbind, plotDF)
plot(plotDF$locPol, plotDF$locWC)
text(plotDF$locPol, plotDF$locWC, plotDF$loc, cex=0.6, pos=4, col="red")

# End