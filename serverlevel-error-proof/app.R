
library(reactable)
library(jsonlite)
library(tm)
library(slam)
library(shiny)
library(shinyjs)

custom_dict = readRDS("custom_dict.rds")
x = readRDS("tdm_model.rds")
y = readRDS("newtdm063cosim.rds")
tbl_ = readRDS("querytbl.rds")

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
  corpus <- tm::VCorpus(tm::VectorSource(qs))
  tdm <- tm::TermDocumentMatrix(corpus, control = list(dictionary = custom_dictionary,
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

ui <- fluidPage(
  title = 'QSearch',
  useShinyjs(),
  
  tags$head(
    title = "Flipside Data Science",
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Questrial"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Inter")
  ),
  withTags({
    header(class="top-banner",
           section(
             a(class="fs-logo", href="https://www.flipsidecrypto.com", img(src = "FLIPSIDE-BRAND-1-WHITE.png", width = "75%")),
             section(class="socials",
                     a(class="twitter", target = "_blank", href="https://twitter.com/flipsidecrypto", "Twitter"),
                     a(class="linkedin", target = "_blank", href="https://www.linkedin.com/company/flipside-crypto", "LinkedIn"),
                     a(class="discord", target = "_blank", href="https://flipsidecrypto.com/discord", "Discord"),
                     a(href="https://next.flipsidecrypto.xyz/", target = "_blank", "Explore our data!")
             )
           )
    )
  }),
  hr(class = "break-line"),
  
  # APP LABEL HERE -----------------------------------  
  
  withTags({
    section(class='hero',
            h1(
              class='header', 
              'Q-Search', 
            ),
            p('Inspiration for your next Flipside Query'),
    )
  }),
  
  # APP START HERE -----------------------------------  
  
  ## EXAMPLE INPUTS DIV ----
  div( # re-using chart classes to make smoother outlining
    class = 'chart-container',
    div(
      class = 'chart-block',
      fluidRow(
        column(8, 
               textAreaInput(inputId = "textarea",
                             label = "Paste SQL or any SQL-ish", 
                             value = "select count num daily users from ethereum fact transactions", 
                             width = "100%", 
                             height = "100%", 
                             rows = 10)
        ),
        column(4, 
               fluidRow(numericInput(inputId = 'nresults',
                                     label = "Max # Results",
                                     value = 10,
                                     min = 1,
                                     max = 50,
                                     step = 1,
                                     width = '80%')
               ),
               fluidRow(actionButton(inputId = 'search',
                                     label = "Search", 
                                     width = "80%")
               )
        )
        
      )
    )),
  
  # EXAMPLE REACTABLE TABLE DIV ----
  # re-using chart classes to make smoother 
  div(
    class = 'chart-container',
    div(
      class = 'chart-block',
      fluidRow(
        column(1, br()),
        column(6, class = "simscore",
               sliderInput(inputId = 'minscore',
                           label = "Min Similarity Score",
                           min = 0,
                           max = 1,
                           value = 0.25,
                           step = 0.05, 
                           width = "80%")
        ), 
        column(5, 
               selectInput(inputId = "tablefilter", 
                           label = "Filter by Table Used",
                           choices = NULL,
                           selected = NULL,
                           multiple = FALSE, # 1 at a time
                           selectize = TRUE,
                           width =  "80%")
        )
      ),
      p(class = 'fork', "Select an ID to Fork the Query on the Flipside Data App"),
      hr(),
      reactableOutput("results_reactbl")
    )
  )
  
  
)

server <- function(input, output) {
 
  xx <- eventReactive(input$search, {
    
    new_query <- "select count num daily users from ethereum fact transactions"
    new_query <- input$textarea
    
    # clean for cosine similarity
    nq <- clean_query(new_query)
    
    nqtdm <- generate_tdm_model(nq, custom_dictionary = custom_dict)
    nqtdm$dimnames <- NULL
       
    # IN RConnect the result of termDocumentMatrix
    # has row values shifted down by 1.
    # cannot figure out why
    if(shiny::isRunning()){
    nqtdm$i <- as.integer(nqtdm$i - 1)
    }
    
       cos1 =  calculate_similarity(x, newtdm = nqtdm)
       cos2 =  calculate_similarity(x, newtdm = y)
       
      rn = as.data.frame(as.matrix(nqtdm))
      ry = as.data.frame(as.matrix(y))
      
      data.frame(
        nn = nrow(rn), 
        ny = nrow(ry),
        n1 = nqtdm$i,
        y1 = y$i,
        row.names = NULL
      )
      
      
    
      })
  
  output$results_reactbl <- renderReactable({
   reactable(xx())
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
