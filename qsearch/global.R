library(shinyjs)
library(reactable)
library(httr)
library(jsonlite)

baseurl <- readLines("qmatch_url.txt")

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

