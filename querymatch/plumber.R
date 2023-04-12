
library(plumber)
source("clean_query.R")
source("generate_tdm.R")
source("calculate_similarity.R")

# when updating to a new model 
# update the testshift accordingly 
# as Linux RConnect may shift sparse matrix i values by +1
# which causes a problem in cosine similarity.

# It is known that query row 76286, corresponding 
# with column (Doc) 76286 in the trained tdm_model 
# has a cosine similarity with the test input of 0.63 
# update as new models are generated 
test_row = 76286
test_input = "select count num daily users from ethereum fact transactions"
test_target = 0.63

custom_dict <- readRDS("custom_dict.rds")
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

#* Return the top N rows for querytbl from model matching query
#* @param querytext The SQL query seeking its match. 
#* @param n The N top results to return.
#* @post /querymatch
function(querytext, n = 10, shift = TRUE) {
  
  # clean for cosine similarity
  nq <- clean_query(querytext)
  nqtdm <- generate_tdm_model(nq, custom_dictionary = custom_dict)
  
  if(shift){
    nqtdm$i <- as.integer(nqtdm$i - 1)
  }
  
  cosims <- calculate_similarity(select_tdm, nqtdm)
  
  topn = order(cosims$cosim, decreasing = TRUE)[1:n]
  
  res_ = querytbl[topn, ]
  res_$score <- cosims$cosim[topn]
  
return(
  res_
)
 
}

#* Return the key model details
#* @post /testshift 
function(shift = FALSE) {

  nq <- clean_query(test_input)
  nqtdm <- generate_tdm_model(nq, custom_dictionary = custom_dict)
  if(shift){
    nqtdm$i <- as.integer(nqtdm$i - 1)
  }
  mat1 <- select_tdm[, test_row]
  
   x = round(calculate_similarity(mat1, nqtdm)$cosim, 2)
   
  if(x == test_target){
    return(TRUE)
  } else { 
    return(FALSE)
    }
  
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
