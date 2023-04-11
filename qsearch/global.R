
source("clean_query.R")
source("generate_tdm.R")
source("calculate_similarity.R")
library(shinyjs)
library(htmltools)
library(reactable)
library(httr)
library(jsonlite)
querytbl <- readRDS("querytbl.rds")
select_tdm <- readRDS("tdm_model.rds")

baseurl <- readLines("qmatch_url.txt")

qsearch <- function(website_base_url, query_text, n = 10){
  url <- paste0(website_base_url, "querytext=", URLencode(query_text, reserved = TRUE), "&n=", n)
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

get_top <- function(querytext, n = 10) {
  
  # clean for cosine similarity
  nq <- clean_query(querytext)
  nqtdm <- generate_tdm_model(nq, custom_dictionary = Terms(select_tdm))
  cosims <- calculate_similarity(select_tdm, nqtdm)
  
  response <- querytbl
  response$score <- cosims$cosim
  response <- response[order(response$score, decreasing = TRUE)[1:n], ]
  
  return(response)
  
}
