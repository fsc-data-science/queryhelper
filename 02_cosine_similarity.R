library(tm)
library(slam)
library(dplyr)
source("clean_query.R")
querytbl <- readRDS("select_querytbl.rds")
select_tdm <- readRDS("select_tdm_model.rds")

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

calc_cos_sims <- function(tdm_model, newtdm){
  
  dot_products <- row_sums(crossprod_simple_triplet_matrix(tdm_model, newtdm))
  magnitudes <- sqrt(col_sums(newtdm^2)) * sqrt(col_sums( tdm_model  ^ 2))
  similarities <- dot_products / magnitudes
  
  df <- data.frame(
    docs = names(similarities),
    index = 1:length(similarities),
    cosim = as.numeric(similarities), 
    row.names = NULL
  )
  
  return(df)
}

res <- calc_cos_sims(select_tdm, nqtdm)


