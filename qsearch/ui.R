library(shiny)
source("global.R")


# Define UI ------

shinyUI(fluidPage(
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
                             label = "Text Area", 
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
      div(class = 'chart-title', span('Reactable Table EXAMPLE'),
      
      fluidRow(
        column(6, 
               sliderInput(inputId = 'minscore',
                           label = "Min Similarity Score",
                           min = 0.05,
                           max = 1,
                           value = 0.25,
                           step = 0.05, 
                           width = "80%")
        ), 
        column(6, 
               selectInput(inputId = "tablefilter", 
                           label = "Filter by Table Used",
                           choices = NULL,
                           selected = NULL,
                           multiple = FALSE, # 1 at a time
                           selectize = TRUE,
                           width =  "80%")
               )
      )),
      reactableOutput("myreactable")
      )
    )
  

) # end FluidPage
) # end shinyUI