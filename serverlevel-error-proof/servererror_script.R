library(tm)
library(slam)

calculate_similarity <- function(tdm_model, newtdm){
  
  dot_products <- slam::row_sums(crossprod_simple_triplet_matrix(tdm_model, newtdm))
  magnitudes <- sqrt(slam::col_sums(newtdm^2)) * sqrt(slam::col_sums( tdm_model  ^ 2))
  similarities <- dot_products / magnitudes
  
  df <- data.frame(
    index = 1:length(similarities),
    cosim = as.numeric(similarities), 
    row.names = NULL
  )
  
  return(df)
}

x = readRDS("tdm76286.rds")
y = readRDS("newtdm063cosim.rds")

# if 0.631 then correct 
# if 0 then dot product is not working correctly 
calculate_similarity(x, y)

# should be 37 
# if 0 then server wrong 
crossprod_simple_triplet_matrix(x,y)
