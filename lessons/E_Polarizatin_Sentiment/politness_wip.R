#' Politness Example
#' Jan 29 2021
#' 
# https://cran.r-project.org/web/packages/politeness/vignettes/politeness.html

# Sometimes problematic; authors are using a python lib
# install.packages("spacyr")
# https://bic-berkeley.github.io/psych-214-fall-2016/using_pythonpath.html
# spacyr::spacy_initialize(python_executable = "PYTHON_PATH")

# libs
library(politeness)

# data
data("phone_offers")
data('bowl_offers')

# Frequentist annotation
df_politeness_count <- politeness(phone_offers$message, 
                                  drop_blank = T,
                                  metric     = "count") # "binary" # metric="binary"

# Standard plot; I don't find it intuitive
politenessPlot(df_politeness_count,
               split=phone_offers$condition,
               split_levels = c("Tough","Warm"),
               split_name = "Condition")

# According to their annotation methods find the most or least polite requires the construction of a classifier
df_polite_holdout <- politeness(bowl_offers$message)
df_polite_train   <- politeness(phone_offers$message, metric = 'binary')
project           <- politenessProjection(df_polite_train,
                                          phone_offers$condition,
                                          df_polite_holdout)



fpt_most          <- findPoliteTexts(phone_offers$message,
                                     df_polite_train,
                                     phone_offers$condition,
                                     type="most")
fpt_least        <- findPoliteTexts(phone_offers$message,
                                    df_polite_train,
                                    phone_offers$condition,
                                    type="least")


# End