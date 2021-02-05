#' Title: Spell Check Examples
#' Purpose: Run through basic spell checking of text
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Dec 30 2020
#'

# Libs
library(qdap) #pg 45 in book has another option but crashes rstudio free cloud instances
library(spelling)
library(hunspell)
library(mgsub)
library(pbapply)
library(dplyr)

# List available dictionaries; not great
# https://cloud.r-project.org/web/packages/hunspell/vignettes/intro.html#hunspell_dictionaries
hunspell:: list_dictionaries()

# Get some text
exampleTxtA <- 'i luve coffe'
exampleTxtB <- "coffee is wunderful"
exampleTxtC <- 'I luve coffee'
allTxt      <- c(exampleTxtA, exampleTxtB, exampleTxtC)

# Declare any words you want to ignore, ie twitters "RT" or "SMH"
ignoreWords = c('luve')

# Identify mispelled words
#spell_check_files(path, ignore = ignoreWords, lang = "en_US") #check files on disk for formats: markdown, but also latex, html, xml, pdf, and plain text
spell_check_text(allTxt, lang = "en_US")
spell_check_text(allTxt, ignore = ignoreWords, lang = "en_US")


# No good correction functions, book pg 46
(mispelled <- spell_check_text(allTxt, lang = "en_US"))
(corrected <- hunspell_suggest(mispelled$word))

# Review
mispelled
corrected

# Add word boundaries around the correction lexicon
correctionLexicon <- data.frame(wrong = paste0('\\b', mispelled$word,'\\b'),
                                right = c(corrected[[1]][1],
                                          corrected[[2]][6],
                                          corrected[[3]][1]))

correctionLexicon

# Loop each doc through all possible mispellings, another method could use mispelled$found 
correctedTxt <- vector()
for( i in 1:length(allTxt)){
  print(paste('correcting', i))
  txt <- allTxt[i]
  for(j in 1:nrow(correctionLexicon)){
    txt <- gsub(correctionLexicon$wrong[j],
                correctionLexicon$right[j],
                txt)
  }
  correctedTxt[i] <- txt
}
correctedTxt

# Or use mgsub
correctedTxt2 <- pblapply(allTxt, mgsub, correctionLexicon$wrong, correctionLexicon$right)
correctedTxt2 <- do.call(rbind, correctedTxt2)
correctedTxt2

# Or use qdap
x <- "Robots are evl creatres and deserv exterimanitation."
which_misspelled(x, suggest=FALSE)
which_misspelled(x, suggest=TRUE)
check_spelling(DATA$state)
check_spelling(x)

# There may be a better way but this is one method to auto correct
allTxt <- c(allTxt, x)
allTxt


correctedTxt <- vector()
for(i in 1:length(allTxt)){
  print(i)
  txtString <- allTxt[i]
  tokens <- which_misspelled(txtString, suggest = T)[,1:3]
  tmp    <- strsplit(txtString, ' ')[[1]] #split individual words
  tmp    <- data.frame(idx = as.character(1:length(tmp)),
                       word = tmp)
  tmp    <- left_join(tmp, tokens, by = c('idx'='word.no'))
  corrected <- ifelse(is.na(tmp$not.found),tmp$word, tmp$suggestion)
  corrected <- paste(corrected, collapse = ' ')
  correctedTxt[i] <- corrected
}
correctedTxt

# End