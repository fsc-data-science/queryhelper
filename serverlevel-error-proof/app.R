

library(reactable)
library(jsonlite)
library(tm)
library(slam)
library(shiny)
library(shinyjs)

x = readRDS("tdm_model.rds")
y = readRDS("newtdm063cosim.rds")

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

generate_tdm_model <- function(queries, 
                               custom_dictionary = NULL,
                               removpunc = TRUE,
                               removnum = TRUE,
                               stops = TRUE){
  qs <- queries
  corpus <- VCorpus(VectorSource(qs))
  tdm <- TermDocumentMatrix(corpus, control = list(dictionary = custom_dictionary,
                                                   tolower = TRUE, 
                                                   removePunctuation = removpunc, 
                                                   removeNumbers = removnum,
                                                   stopwords = stops))
  
  return(tdm)
  
}

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

library(shiny)
ui <- fluidPage(
  actionButton("submit", "Run"),
  dataTableOutput("outp")
)

server <- function(input, output) {
 
  xx <- eventReactive(input$submit, {
    
    new_query <- "select count num daily users from ethereum fact transactions"
    
    # clean for cosine similarity
    nq <- clean_query(new_query)
    
    nqtdm <- generate_tdm_model(nq, custom_dictionary = Terms(x))
    calculate_similarity(x,y)
    
  })
  
  output$outp <- renderDataTable({
    xx()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
