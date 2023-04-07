library(plotly)
library(slam)

select_tdm <- readRDS("select_tdm_model.rds")

freq_ <- as.numeric(row_sums(select_tdm))
doc_ <- as.numeric(col_sums(select_tdm))

  
rangesteps <- seq(from = 0, to = 250, by = 5)
cdf_ <- ecdf(freq_)(rangesteps)
cdf_doc_ <- ecdf(doc_)(rangesteps)

plot_ly(data = data.frame(), x = ~rangesteps, y = ~cdf_, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "Frequency of term across 130,000+ SQL queries"),
         yaxis = list(title = "Cumulative % of term-frequency"),
         title = list(
           text = "75% of model terms appear under 25 times total", 
           y = 0.95)
  )

plot_ly(data = data.frame(), x = ~rangesteps, y = ~cdf_doc_,
        type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "# of Terms"),
         yaxis = list(title = "Cumulative % of doc-term count"),
         title = list(
           text = "80% of docs hav <50 model terms total", 
           y = 0.95)
  )
