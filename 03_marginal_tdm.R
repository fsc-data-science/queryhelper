library(tm)
library(slam)
library(proxy)
library(Matrix)
library(dplyr)

source("clean_query.R")
queries <- readRDS("cleaned_queries.rds")[2000:3000, ]

# Big - must be retrained regularly 
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

#' 1001 large TDM
#' punctuation, numbers, stopwords removed 
#' Takes several minutes to build !! 

qtdm_TTT <- generate_tdm_model(queries = queries$query,
                               custom_dictionary = NULL,
                                    removpunc = TRUE, 
                                    removnum = TRUE,
                                    stops = TRUE)


# Reduce to words appearing at least 2x 
# AND weight rarer word co-occurrence.

qtdm_TTT_2idf <- tm::weightTfIdf(
  qtdm_TTT[which(row_sums(qtdm_TTT) >= 2), ]
  )
  
#' 1001 large TDM
#' numbers removed, stopwords & punctuation kept 
#' Takes several minutes to build !!
qtdm_FTF <- generate_tdm_model(queries = queries$query,
                               custom_dictionary = NULL,
                               removpunc = FALSE, 
                               removnum = TRUE,
                               stops = FALSE)

qtdm_FTF_2idf <- tm::weightTfIdf(
  qtdm_FTF[which(row_sums(qtdm_FTF) >= 2), ]
)

new_query <- {
  "
  -- daily token transfer amounts by num transfers on optimism
  SELECT DATE_TRUNC( 'day', BLOCK_TIMESTAMP) as day_,
   CONTRACT_ADDRESS, 
  count(*) as n_transfers,
  sum(raw_amount) as transfer_amt
    FROM optimism.core.fact_token_transfers
  GROUP BY 1,2
  ORDER BY day_ asc, n_transfers DESC
  ;
"
}

# clean for cosine similarity
nq <- clean_query(new_query)

#' Given a known termDocumentMatrix
#' and a new query 
#' assess the cosine similarity of the new query against
marginal_cosim <- function(new_query, tdm_model, 
                           removpunc = FALSE, 
                           removnum = TRUE,
                           stops = FALSE,
                           idf = TRUE){
browser()
   nq_tdm <- generate_tdm_model(queries = new_query,
                              custom_dictionary = Terms(tdm_model),
                              removpunc = removpunc, 
                              removnum = removnum,
                              stops = stops)
   
 if(idf){
   nq_tdm <- tm::weightTfIdf(nq_tdm)
 }
 
cosine_dist <- proxy::dist(nq_tdm, tdm_model, method = "cosine")
 
 # 1 = similarity+distance
 similarities <- 1 - cosine_dist
 
  return(similarities)
  
}

cs_ttt <- marginal_cosim(new_query = nq, 
                         tdm_model = qtdm_TTT_2idf,
                         removpunc = TRUE, 
                         removnum = TRUE,
                         stops = TRUE,
                         idf = TRUE)

cs_ftf <- marginal_cosim(nq, qtdm_FTF_2idf,
                         removpunc = FALSE, 
                         removnum = TRUE,
                         stops = FALSE)

