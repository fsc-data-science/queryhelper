library(tidytext)
library(dplyr)
library(furrr)
queries <- readRDS("cleaned_queries.rds")


unique_tbls <- paste0(unique(queries$TABLES), collapse = ",")
unique_tbls <- gsub("\n|\\[|\\]"," ", unique_tbls)
unique_tbls <- strsplit(unique_tbls, split = ",", perl = TRUE)

unique_tbls <- lapply(unique_tbls, FUN = function(x){gsub("\\s+|\\\"", "", x)})

tbl_counts <- as.data.frame(table(unlist(unique_tbls)))
