library(tm)
library(dplyr)
source("clean_query.R")
queries <- readRDS("cleaned_queries.rds")

# Function to create a term-document matrix for two SQL queries
create_tdm <- function(new_query, relevant_query_history) {
  qs <- c(new_query, relevant_query_history)
  corpus <- VCorpus(VectorSource(qs))
  tdm <- TermDocumentMatrix(corpus, control = list(tolower = FALSE, 
                                                   removePunctuation = FALSE, 
                                                   removeNumbers = FALSE,
                                                   stopwords = FALSE))
  return(tdm)
}

cosine_similarity <- function(query_1, query_2) {
  tdm <- create_tdm(query_1, query_2)
  tdm_matrix <- as.matrix(tdm)
  
  dot_product <- tdm_matrix[, 1] %*% tdm_matrix[, 2]
  query1_magnitude <- sqrt(sum(tdm_matrix[, 1] ^ 2))
  query2_magnitude <- sqrt(sum(tdm_matrix[, 2] ^ 2))
  similarity <- as.numeric(dot_product / (query1_magnitude * query2_magnitude))
  
    return(similarity)
}

cosine_similarity_bulk <- function(new_query, relevant_query_history) {
  browser()
  tdm <- create_tdm(new_query, relevant_query_history)
  tdm_matrix <- as.matrix(tdm)
  new_qvec <- as.matrix(tdm_matrix[, 1])
  qvec_history <- as.matrix(tdm_matrix[, -1]) 
  
  # new_qvec is a T x 1 matrix (terms rows, 1 column)
  # qvec_history is T x N matrix (terms rows, N columns)
  # transpose qvec_history to get: 
  # [T x N]^-1 %*% [T x 1]
  # [N x T] %*% [T x 1] = dot-product
  dot_product <- t(qvec_history) %*% new_qvec 
  
  # similarly, transpose the square prior to rowSums 
  # to get magnitude vector for calculating similarity.
  magnitudes <- sqrt( rowSums(t(qvec_history ^ 2)) * sqrt(sum(new_qvec ^ 2)))
  similarities <- as.numeric(dot_products / magnitudes)
  
  return(similarity)
}


op_transfers_queries <- queries %>% filter(grepl('optimism.core.fact_token_transfers', TABLES))

# smaller sample 
op_transfers_queries <- op_transfers_queries[1:100,]

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

cs <- list()
for(i in 1:nrow(op_transfers_queries)){
  cs[i] <- cosine_similarity(nq, op_transfers_queries$query[i])
}

cs2 <- cosine_similarity_bulk(nq, op_transfers_queries$query)

hist(unlist(cs), breaks = 10)

top_index <- which(unlist(cs) >= 0.5)

results <- op_transfers_queries[top_index, ]
results$cosine_score <- unlist(cs)[top_index]

View(results)
