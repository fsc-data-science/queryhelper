
library(plumber)
source("clean_query.R")
source("generate_tdm.R")
source("calculate_similarity.R")
querytbl <- readRDS("querytbl.rds")
select_tdm <- readRDS("tdm_model.rds")

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
  cosims <- calculate_similarity(select_tdm, nqtdm)
  
  topn = order(cosims$cosim, decreasing = TRUE)[1:n]
  
return(
  data.frame(
    index = topn,
    cosim = cosims$cosim[topn]
  )
  
)
 
}

#* Return the key model details
#* @post /modeldetails
function() {

  nq <- clean_query('select count num daily users from ethereum fact transactions')
  nqtdm <- generate_tdm_model(nq, custom_dictionary = Terms(select_tdm))
  cosims <- calculate_similarity(select_tdm, nqtdm)
  dot_products = slam::row_sums(crossprod_simple_triplet_matrix(select_tdm, nqtdm))
  
mat1 <- select_tdm[, 76286]
mat2 <- nqtdm 

  return(list(
    mean = mean(dot_products),
    med = median(dot_products),
    n0 = sum(dot_products == 0),
    max = max(dot_products),
    meanrr = mean(row_sums(select_tdm)),
    meancc = mean(col_sums(select_tdm)),
    testcs = calculate_similarity(mat1, mat2),
    testdp = slam::row_sums(crossprod_simple_triplet_matrix(mat1, mat2))
  )
  )
  
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
