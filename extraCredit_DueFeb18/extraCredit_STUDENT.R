#' ENTER YOUR NAME HERE
#' Extra Credit
#' Feb11
#' 

# Set the working directory


# Libs: tm, qdap, ggplot2, ggthemes, ggdendro


# Options & Functions; stringsAsFactors = FALSE; Sys.setlocale('LC_ALL','C')


# Add try to lower custom function

# add CleanCorpus with removeNumbers, removePunctuation, stripWhitespace, content_transformer(tryTolower), [removeWords, customStopwords], content_transformer(qdapRegex::rm_url), content_transformer(replace_contraction)

# Create custom stop words with the SMART lexicon and 'amp','PR','iphone' in an objeect called stops
 

# Read in Data, clean & organize with read.csv, VCorpus, VectorSource using text$completeTxt, cleanCorpus with stops, and make a TDM
text      <- read.csv('PressReleases.csv')

# Create word associations for "android" with a 0.5 cutoff in an object called associations


# Examine associations object


# Reduce TDM with removeSparseTerms and a sparse value =0.75 in an object reducedTDM

# Examine the reducedTDM



# Change the reducedTDM object with as.matrix inside of as.data.frame


# Calculate the reducedTDMdistances in an object called hc


# Use hclust to get hierarchical clusters for hc, re-declaring the object as hc


# Plot HC
plot(hc,yaxt='n')

# End