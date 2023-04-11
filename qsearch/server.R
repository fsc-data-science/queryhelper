library(shiny)
source("global.R")
server <- function(input, output, session) {

  prev_result <- reactiveVal(value = list()) 
  error_track <- reactiveVal(0)
  
  observeEvent(list(input$search, input$textarea, prev_result(), error_track()), {
    prevr <<- prev_result()
    et <<- error_track()
    ta <<- qtext()
    sr <<- search_results()
    mt <<- match_tbl()
  })
  
  qtext <- reactive({
   clean_query(shQuote(input$textarea))
  })
  
  search_results <- eventReactive(input$search, {
     
          resp <- prev_result()
      withProgress(message = "Searching", detail = "", value = 0, 
                   expr = {
                     incProgress(0.1)
                     incProgress(0.25)
      newsrch <- get_top(querytext = qtext(), n = input$nresults)
      nn <<- newsrch
                      incProgress(detail = "Calculating Similarity",
                                  amount = 0.65)
      
                   })
      
      if(nrow(newsrch) > 0){
        resp <- newsrch
        prev_result(resp)
        error_track(0)
      } else { 
        error_track(newsrch)
      }
    })
  
  match_tbl <- eventReactive(prev_result(), {
    prev_result()
  })
  
  observeEvent(match_tbl(), {
    
    tabls_ <- unique(clean_tbl_names(match_tbl()$TABLES))
    
    updateSelectInput(session = session, 
                      inputId = "tablefilter", 
                label = "Filter by Table Used",
                selected = "", 
                choices = c(tabls_, ".")
    )
    
  })
  
  select_tbl <- eventReactive(list(match_tbl(), 
                                   input$minscore, 
                                   input$tablefilter),
                              {
                                
     mt <- match_tbl()
     mt$TABLES <- lapply(mt$TABLES, clean_tbl_names)
     mt$score <- round(mt$score, digits = 2)
     
     # 1 table at a time only
     # or else AND/OR rules would complicate this
     tblfilter_index <- unlist(
       lapply(mt$TABLES, 
              function(x){ 
                mean(grepl(input$tablefilter, x)) > 0   }
       ))
     
     mt[mt$score >= input$minscore & tblfilter_index, c("QUERY_ID","NAME","score","STATEMENT", "NCHAR","QUERY_TIME")]
     
     })
  
  output$results_reactbl <- renderReactable({
       reactable(select_tbl(), 
              columns = list(
      QUERY_ID = colDef(minWidth = 25,
                  cell = link_renderer),
      NAME = colDef(minWidth = 25),
      score = colDef(minWidth = 15),
      STATEMENT = colDef(cell = preformatted_cell_renderer, minWidth = 100),
      NCHAR = colDef(minWidth = 15),
      QUERY_TIME = colDef(minWidth = 25)
    ))
    
  })
  
}