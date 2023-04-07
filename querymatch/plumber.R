
library(plumber)
library(tm)
library(slam)
source("clean_query.R")
source("generate_tdm.R")
source("calculate_similarity.R")
querytbl <- readRDS("select_querytbl.rds")
select_tdm <- readRDS("select_tdm_model.rds")

#* @apiTitle Query Match
#* @apiDescription Discover queries similar to yours.

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Return the top N of querytbl matching query
#* @param querytext The SQL query seeking its match. 
#* @param n The N top results to return.
#* @post /querymatch
function(querytext, n = 10) {
  
  # clean for cosine similarity
  nq <- clean_query(querytext)
  nqtdm <- generate_tdm_model(nq, custom_dictionary = Terms(select_tdm))
  res <- calculate_similarity(select_tdm, nqtdm)
  
 response <- querytbl[res$index[order(res$cosim, decreasing = TRUE)[1:n]], ]
 response$score <- res$cosim[order(res$cosim, decreasing = TRUE)[1:n]]

return(response)
 
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
