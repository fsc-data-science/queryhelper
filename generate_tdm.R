library(tm)
library(slam)

generate_tdm_model <- function(queries, 
                               custom_dictionary = NULL,
                               removpunc = TRUE,
                               removnum = TRUE,
                               stops = TRUE){
  qs <- queries
  corpus <- VCorpus(VectorSource(qs))
  tdm <- TermDocumentMatrix(corpus, control = list(dictionary = custom_dictionary,
                                                   tolower = TRUE, 
                                                   removePunctuation = removpunc, 
                                                   removeNumbers = removnum,
                                                   stopwords = stops))
  
  return(tdm)
  
}