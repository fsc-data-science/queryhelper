library(tm)
library(slam)
library(dplyr)

source("clean_query.R")
queries <- readRDS("cleaned_queries.rds")

# Big - must be retrained regularly 
generate_tdm_model <- function(queries, 
                               term_frequency_minimum = 20,
                       removpunc = TRUE,
                       removnum = TRUE,
                       stops = TRUE){
  qs <- queries
  corpus <- VCorpus(VectorSource(qs))
  tdm <- TermDocumentMatrix(corpus, control = list(tolower = TRUE, 
                                                   removePunctuation = removpunc, 
                                                   removeNumbers = removnum,
                                                   stopwords = stops))
  return(tdm)

}

#' 243, 961 large TDM
#' punctuation, numbers, stopwords removed 
#' Takes several minutes to build !! 

qtdm_TTT <- generate_tdm_model(queries = queries$query,
                                    removpunc = TRUE, 
                                    removnum = TRUE,
                                    stops = TRUE)


# Reduce to words appearing at least 200x 
# AND weight rarer word co-occurrence.

qtdm_TTT_200idf <- tm::weightTfIdf(
  tdm[which(row_sums(tdm) >= 200), ]
  )
  

#' 243, 961 large TDM
#' numbers removed, stopwords & punctuation kept 
#' Takes several minutes to build !!
qtdm_FTF <- generate_tdm_model(queries = queries$query,
                               removpunc = FALSE, 
                               removnum = TRUE,
                               stops = FALSE)

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
                           stops = FALSE){
 nq <- new_query
   
 nq_corp <- VCorpus(VectorSource(nq))
 
 # re-use terms from previously existing model
 nq_tdm <- TermDocumentMatrix(nq_corp, 
                              control = list(dictionary = Terms(tdm_model), 
                                             tolower = TRUE, 
                                             removePunctuation = removpunc, 
                                             removeNumbers = removnum,
                                             stopwords = stops))
 
 qvec_history <- as.matrix(tdm_model)
 new_qvec <- as.matrix(nq_tdm)
 
 # new_qvec is a T x 1 matrix (terms rows, 1 column)
 # qvec_history is T x N matrix (terms rows, N columns)
 # transpose qvec_history to get: 
 # [T x N]^-1 %*% [T x 1]
 # [N x T] %*% [T x 1] = dot-product
 
 dot_products <- t(qvec_history) %*% new_qvec 
 
 # similarly, transpose the square prior to rowSums 
 # to get magnitude vector for calculating similarity.
 magnitudes <- sqrt(rowSums(t(qvec_history ^ 2))) * sqrt(sum(new_qvec ^ 2))
 similarities <- as.numeric(dot_products / magnitudes)
 
 return(similarities)
  
}


cs_ttt <- marginal_cosim(nq, qtdm_TTT,
                         removpunc = TRUE, 
                         removnum = TRUE,
                         stops = TRUE)

cs_FTF <- marginal_cosim(nq, qtdm_FTF,
                         removpunc = FALSE, 
                         removnum = TRUE,
                         stops = FALSE)
