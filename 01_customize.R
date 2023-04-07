library(dplyr)
source("generate_tdm.R")

queries <- readRDS("cleaned_queries.rds")

# generate termDocumentMatrix for cleaned queries 
total_tdm <- generate_tdm_model(queries$query, NULL, TRUE, TRUE, TRUE)

termfreq <- data.frame(
  term = Terms(total_tdm),
  n = row_sums(total_tdm),
  m = row_means(total_tdm),
  row.names = NULL
)

docfreq <- data.frame(
  docs = Docs(total_tdm),
  n = col_sums(total_tdm),
  m = col_means(total_tdm)
) 

# term needs to appear 5+ times
# query needs to have at least 10 terms 
select_tdm <- total_tdm[termfreq$n >= 5, docfreq$n >= 10] 

# slight cleanup to remove terms who lost their only docs 
select_tdm <- select_tdm[-which(row_sums(select_tdm) == 0), ]

# Only these queries got included
# have to use as.numeric to grab their Doc "names" for filtering 
querytbl <- queries[as.numeric(select_tdm$dimnames$Docs),  ]

saveRDS(querytbl, "select_querytbl.rds")
saveRDS(select_tdm, "select_tdm_model.rds")



