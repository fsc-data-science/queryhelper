library(dplyr)
source("generate_tdm.R")

queries <- readRDS("cleaned_queries.rds")

clean_tbl_names <- function(tblnames){
  tblnames <- gsub(pattern = "\\\n| |\\\t |\\[|\\]|\"", replacement = "", x = tblnames)
  tblnames <- unlist(strsplit(tblnames, ","))
  return(tblnames)
}

queries$TABLES <- lapply(queries$TABLES, clean_tbl_names)
# Filter out use of extremely rare tables 
# may be deprecated or erroneous 

tbl_list <- as.data.frame(table(unlist(queries$TABLES)))

# table must be used at least 5 times 
cuttbls <- tbl_list$Var1[tbl_list$Freq < 5] 

cutrows <- unlist(lapply(queries$TABLES, function(x){
  mean(cuttbls %in% x) > 0
}))

queries <- queries[!cutrows, ]

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



