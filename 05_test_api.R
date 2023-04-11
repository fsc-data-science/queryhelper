# API Test 
library(httr)
library(jsonlite)
source("clean_query.R")
# API Model
baseurl <- readLines("qmatch_url.txt")

qsearch <- function(website_base_url, query_text, n = 10){
  url <- paste0(website_base_url, "querytext=", URLencode(query_text, reserved = TRUE), "&n=", n)
  headers <- add_headers("accept" = "*/*")
  response <- POST(url, headers, body = "")
  
  return(response)
}

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

newsrch <- qsearch(baseurl, query_text = new_query, n = 10)
resp <- httr::content(newsrch)
match_tbl <- do.call(rbind.data.frame, resp)
