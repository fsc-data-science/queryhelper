
library(plumber)
source("clean_query.R")
source("generate_tdm.R")
source("calculate_similarity.R")
library(tm)
library(slam)
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
#* @post /qmatch
function(querytext, n = 10) {
  
  # clean for cosine similarity
  nq <- clean_query(querytext)
  nqtdm <- generate_tdm_model(nq, custom_dictionary = Terms(select_tdm))
  cosims <- calculate_similarity(select_tdm, nqtdm)
  
 response <- querytbl
 response$score <- cosims$cosim
 response <- response[order(response$score, decreasing = TRUE)[1:n], ]

return(response)
 
}

#* Return the key model details
#* @post /modeldetails
function() {

  # Create two matrices as simple_triplet_matrix objects
  mat1 <- simple_triplet_matrix(
    i = c(1, 2, 3),
    j = c(1, 2, 3),
    v = c(1, 2, 3)
  )
  
  mat2 <- simple_triplet_matrix(
    i = c(1, 2, 3),
    j = c(1, 2, 3),
    v = c(4, 5, 6)
  )
  
  # Compute the cross-product using slam::crossprod_simple_triplet_matrix()
  crossprod_simple_triplet_matrix(mat1, mat2)
  
  return(list(
    qtbl_rows = nrow(querytbl),
    tdm_docs = length(Docs(select_tdm)),
    correct = cosims[76286, ],
    slam = as.character(packageVersion('slam')),
    clss = class(select_tdm[, 76286]),
    dim = dim(select_tdm[, 76286]),
    rowsum = sum(slam::row_sums(select_tdm[, 76286])),
    colsum = slam::col_sums(select_tdm[, 76286]),
    nq =sum(row_sums(nqtdm)),
    nqcol =sum(col_sums(nqtdm)),
    single_ = crossprod_simple_triplet_matrix(select_tdm[, 76286], nqtdm),
    dotproducts = slam::row_sums(slam::crossprod_simple_triplet_matrix(select_tdm[, 76286], nqtdm)),
    magnitudes = sqrt(slam::col_sums(nqtdm^2)) * sqrt(slam::col_sums( select_tdm[, 76286]  ^ 2))

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
