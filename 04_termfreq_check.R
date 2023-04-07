library(plotly)
library(slam)
freq_ttt <- as.numeric(row_sums(qtdm_TTT))
freq_ftf <- as.numeric(row_sums(qtdm_FTF))
  
rangesteps <- seq(from = 0, to = 250, by = 5)
cdf_ttt <- ecdf(freq_ttt)(rangesteps)

cdf_ftf <- ecdf(freq_ftf)(rangesteps)
plot_ly(data = data.frame(), x = ~rangesteps, y = ~cdf_ttt, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "Frequency of term across 240,000+ SQL queries"),
         yaxis = list(title = "Cumulative % of term-frequency"),
         title = list(
           text = "75% of terms appear under 20 times total (TTT model)", 
           y = 0.95)
  )

plot_ly(data = data.frame(), x =~rangesteps, y = ~cdf_ftf, type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "Frequency of term across 240,000+ SQL queries"),
         yaxis = list(title = "Cumulative % of term-frequency"),
         title = list(
           text = "86% of terms appear under 20 times total (FTF model)", 
           y = 0.95)
  )

doc_ttt <- as.numeric(col_sums(qtdm_TTT))
cdf_doc_ttt <- ecdf(doc_ttt)(rangesteps)
plot_ly(data = data.frame(), x = ~rangesteps, y = ~cdf_doc_ttt,
        type = 'scatter', mode = 'lines+markers') %>% 
  layout(xaxis = list(title = "# of Terms"),
         yaxis = list(title = "Cumulative % of doc-term count"),
         title = list(
           text = "80% of docs hav <100 terms total (TTT model)", 
           y = 0.95)
  )
