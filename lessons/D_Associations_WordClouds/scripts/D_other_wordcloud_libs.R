#' Title: Other Word Cloud Approaches
#' Purpose: Build a word cloud with bi-grams
#' Author: Ted Kwartler
#' License: GPL>=3
#' Date: Dec 28 2020
#'

# Set the working directory
setwd("~/Desktop/Harvard_NLP_Student/lessons/D_Associations_WordClouds/data")

# Libs
library(tm)
library(echarts4r)
library(ggwordcloud)

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('SMART'), 'airplane', 'british', 'ryan', 'air', 'ryanair', 'flight', 'airline')

# Data; limited to 1000 posts for speed
text <- read.csv('RyanAir.csv')$text
text <- iconv(text, "latin1", "ASCII", sub="")

# Quick remove special characters
text <- gsub("\u00A0", "", text)

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)
#saveRDS(txtCorpus, 'txtCorpus.rds') #if working on big data do this to save time

# Make TDM & convert it to matrix
txtTDM  <- TermDocumentMatrix(txtCorpus)
txtTDM <- as.matrix(txtTDM)

# See a token
txtTDM[2015:2018,117:118]

# Get Row Sums & organize
txtTDMv   <- sort(rowSums(txtTDM), decreasing = TRUE)
txtDF     <- data.frame(word = names(txtTDMv), freq = txtTDMv)
txtDF    <- txtDF[1:100,]

# Echarts4R D3 Viz
txtDF %>% 
  e_color_range(freq, color, colors = c("#59c4e6", "#edafda")) %>% 
  e_charts() %>% 
  e_cloud(word = word, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 100)) %>% 
  e_title("RyanAir Tweets") %>%
  e_tooltip()

txtDF %>% 
  e_color_range(freq, color, colors = c("#59c4e6", "#edafda")) %>% 
  e_charts() %>% 
  e_cloud(word = word, 
          freq = freq, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 100)) %>% 
  e_title("RyanAir Tweets") %>%
  e_tooltip() %>% e_theme("dark")

# ggplot interface; proportionality is diminshed to fit
ggplot(txtDF, aes(label = word, size = freq)) +
  geom_text_wordcloud() +
  theme_minimal()

# ggplot interface; proportionality kept but words that dont fit are piled into the center
ggplot(txtDF, aes(label = word, size = freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

# keep proportionality but drop the non fitting words; with a warning
ggplot(txtDF, aes(label = word, size = freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE)  +
  scale_size_area(max_size = 20) +
  theme_minimal()

# Add some color; More options and code: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html
ggplot(txtDF, aes(label = word, 
                  size = freq,
                  color = factor(sample.int(10, nrow(txtDF), replace = TRUE)))) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 24) +
  theme_minimal()

# End
