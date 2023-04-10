clean_query <- function(querystring){
  # add new line to the end for simplicity 
  qs <- tolower(paste0(querystring, "\n"))
  
  # Remove multi-line comments
  qs <- gsub("(?s)/\\*.*?\\*/", "\n", qs, perl = TRUE)
  
  # Remove single-line comments through new line
  qs <- gsub("--.*?\n", "\n", qs, perl = TRUE)
  
  # Remove single-line comments through new line
  qs <- gsub("//.*?\n", "\n", qs, perl = TRUE)
  
  # Remove miscellaneous blank lines
  qs <- gsub("^\\s*\n", "\n", qs, perl = TRUE)
  
  # swap parentheses for spaces
  qs <- gsub("\\(|\\)", " ", qs, perl = TRUE)
  
  # swap commas backslashes and double quotes for spaces 
  qs <- gsub("\\\"|\\\\|\\,|\"|=", " ", qs, perl = TRUE)
  
  # Remove all blank lines with a simple space
  qs <- gsub("\n", " ", qs)
  
  ## clean up all double spaces
  qs <- gsub("\\s{2,}", " ", qs)
  
  return(qs)
}
