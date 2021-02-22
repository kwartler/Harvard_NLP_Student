#' Title: Dealing with Emoji
#' Purpose: Learn and calculate polarity 
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Feb 20, 2021
#'

# Wd
setwd("~/Desktop/Harvard_NLP_Student/lessons/E_Polarization_Sentiment/data")
library(rtweet) # Get the emoji lexicon or load one manually
library(mgsub)

head(emojis)
emojis$code[2]


# Ignor the Japanese
unicorns$text[c(720, 829)]
# https://twitter.com/massafrancis/status/1363660144379273216

grep(emojis$code[2], unicorns$text)

# 1. Remove: GSUB all non-ascii character; this removes all non-English too
# Regex Exaplantion "^" anything but the 1-127th ASCII character is sub'ed to ""
# Yes I had to look that up :)
gsub("[^\x01-\x7F]", "", unicorns$text[c(720, 829)])

# 1A Remove: Or with qdapRegex, removes emoji's only.  Speacial characters remain
rm_emoticon(unicorns$text[c(720, 829)])

# Remove all
st <- Sys.time()
rmTxt <- gsub("[^\x01-\x7F]", "", unicorns$text)
Sys.time() - st #0.01secs for 1k tweets
rmTxt[c(720, 829)]  


# 2. Substitute them with the lexicon
# Remember mgsub library is text, pattern then replacement!
mgsub(unicorns$text[c(720, 829)], emojis$code, emojis$description)

# Sub all
st <- Sys.time()
subTxt <- mgsub(unicorns$text, emojis$code, emojis$description)
Sys.time() - st #43secs 1k tweets
subTxt[c(720, 829)]  

# Since emojis are often without spaces:
subTxt <- mgsub(unicorns$text, emojis$code, paste0(' ', emojis$description,' '))
subTxt[c(720, 829)] 

# End

