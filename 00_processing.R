# MUST use readr 
# as Snowflake data can download csv with errant {\n \" \"" "\""} and other 
# end of line breaking characters that base read.csv() cannot handle.

library(readr)
library(dplyr)
raw <- readr::read_csv("raw_query_data.csv", skip = 1)

# Cleaning up comments and errant spaces ----

sql_code <- "
//credit to all --first comment -- second comment
--newcomment 
-- new comment again
with a AS (
select 1 from dual),

/* multi 
line
comment 
*/

b AS (
-- new comment
select   2 from        dual
)

--third comment

SELECT * FROM a 
/* random comment */
 UNION     ALL     (SELECT * FROM   b )"

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
  

  # Remove all blank lines with a simple space
  qs <- gsub("\n", " ", qs)
  
  ## clean up all double spaces
  qs <- gsub("\\s{2,}", " ", qs)
  
  return(qs)
}

clean_query(sql_code)

# clean version of queries ---- 

cleaned <- raw %>% rowwise() %>% mutate(
  query = clean_query(STATEMENT),
  nch = nchar(query)
)

cleaned <- cleaned %>% filter(nch > 13)

# Save a parsed down version ---- 

saveRDS(cleaned[ ,c("ID", "NAME", "TABLES","query")], file = "cleaned_queries.rds")
