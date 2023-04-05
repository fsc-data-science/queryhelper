library(tm)
source("clean_query.R")
queries <- readRDS("cleaned_queries.rds")

# Function to create a term-document matrix for two SQL queries
create_tdm <- function(query1, query2) {
  qs <- c(query1, query2)
  corpus <- VCorpus(VectorSource(qs))
  tdm <- TermDocumentMatrix(corpus, control = list(tolower = TRUE, 
                                                   removePunctuation = TRUE, 
                                                   removeNumbers = TRUE,
                                                   stopwords = FALSE))
  return(tdm)
}

cosine_similarity <- function(query1, query2) {
  tdm <- create_tdm(query1, query2)
  tdm_matrix <- as.matrix(tdm)
  
  dot_product <- tdm_matrix[, 1] %*% tdm_matrix[, 2]
  query1_magnitude <- sqrt(sum(tdm_matrix[, 1] ^ 2))
  query2_magnitude <- sqrt(sum(tdm_matrix[, 2] ^ 2))
  similarity <- as.numeric(dot_product / (query1_magnitude * query2_magnitude))
  
  return(similarity)
}

op_transfers_queries <- queries %>% filter(grepl('optimism.core.fact_token_transfers', TABLES))

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
hist(unlist(cs), breaks = 10)

top_index <- which(unlist(cs) >= 0.5)

results <- op_transfers_queries[top_index, ]
results$cosine_score <- unlist(cs)[top_index]

View(results)
