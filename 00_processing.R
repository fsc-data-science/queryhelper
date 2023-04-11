# MUST use readr 
# as Snowflake data can download csv with errant {\n \" \"" "\""} and other 
# end of line breaking characters that base read.csv() cannot handle.

library(readr)
library(dplyr)
source("clean_query.R")
raw <- readr::read_csv("historic_queries_pull.csv")

# Cleaning up comments and errant spaces ----

sql_code <- "
//credit to all --first comment -- second comment
--newcomment 
-- new comment again
with a AS (
select 'a_b_.cc.d  444' from dual),

/* multi 
line
comment 
*/

b AS (
-- new comment
select   'a(b)_.cc.d--xx' from        dual
)

--third comment

SELECT * FROM a 
/* random comment */
 UNION     ALL     (SELECT * FROM   b )"


clean_query(sql_code)

# clean version of queries ---- 

cleaned <- raw %>% rowwise() %>% mutate(
  query = clean_query(STATEMENT)
)

# queries have to have select and from or assume they are broken
# also throw out excessively long ones 
cleaned <- cleaned %>% 
  filter(grepl('select', query) & grepl('from', query)) %>% 
  filter(nchar(query) < 1000)

# Save a parsed down version ---- 

saveRDS(cleaned, file = "cleaned_queries.rds")
