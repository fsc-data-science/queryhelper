library(tm)
library(slam)

calculate_similarity <- function(tdm_model, newtdm){
  
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