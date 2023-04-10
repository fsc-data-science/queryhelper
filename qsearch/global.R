library(shinyjs)
library(htmltools)
library(reactable)
library(httr)
library(jsonlite)

baseurl <- readLines("qmatch_url.txt")

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
  
  # swap commas for spaces 
  qs <- gsub("\\,", " ", qs, perl = TRUE)
  
  # Remove all blank lines with a simple space
  qs <- gsub("\n", " ", qs)
  
  ## clean up all double spaces
  qs <- gsub("\\s{2,}", " ", qs)
  
  return(qs)
}

qsearch <- function(website_base_url, query_text, n = 10){
  url <- paste0(website_base_url, "querytext=", URLencode(query_text), "&n=", n)
  headers <- add_headers("accept" = "*/*")
  response <- POST(url, headers, body = "")
  
  return(response)
}

clean_tbl_names <- function(tblnames){
 tblnames <- gsub(pattern = "\\\n| |\\\t |\\[|\\]|\"", replacement = "", x = tblnames)
 tblnames <- unlist(strsplit(tblnames, ","))
  return(tblnames)
}

# Custom cell renderer
preformatted_cell_renderer <- function(value) {
  htmltools::tags$div(style = "white-space: pre-wrap;", value)
}

link_renderer <- function(id) {
    htmltools::tags$a(href = paste0("https://flipsidecrypto.xyz/edit/queries/",id),
                    target = "_blank",
                    as.character(id))
}
