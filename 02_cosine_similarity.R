library(tm)
library(slam)
source("clean_query.R")
source("calculate_similarity.R")
source("generate_tdm.R")
# LOCAL Model 
querytbl <- readRDS("querytbl.rds")
select_tdm <- readRDS("tdm_model.rds")

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

nqtdm <- generate_tdm_model(nq, custom_dictionary = Terms(select_tdm))
res <- calculate_similarity(select_tdm, nqtdm)

n =  10

response <- querytbl[res$index[order(res$cosim, decreasing = TRUE)[1:n]], ]
response$score <- res$cosim[order(res$cosim, decreasing = TRUE)[1:n]]
response


