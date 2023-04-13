
baseurl <- readLines("qmatch_url.txt")

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

clean_tbl_names <- function(tblnames){
  tblnames <- gsub(pattern = "\\\n| |\\\t |\\[|\\]|\"", replacement = "", x = tblnames)
  tblnames <- unlist(strsplit(tblnames, ","))
  return(tblnames)
}

# Custom cell renderer
preformatted_cell_renderer <- function(value) {
    x = htmltools::tags$div(class = "sql-code",
                         style = "white-space: pre-wrap;", sql_highlight(value))
    htmltools::HTML(as.character(x))
}

link_renderer <- function(id) {
  htmltools::tags$a(href = paste0("https://flipsidecrypto.xyz/edit/queries/",id),
                    target = "_blank",
                    as.character(id))
}

qsearch <- function(website_base_url, query_text, n = 10, shift = TRUE){
  url <- paste0(website_base_url, "querytext=", URLencode(query_text, reserved = TRUE), 
                "&n=", n, "%shift=", shift)
  headers <- add_headers("accept" = "*/*")
  response <- POST(url, headers, body = "")
  
  return(response)
}

sql_highlight <- function(sql_code) {
  # Define the SQL keywords and their associated CSS classes
  keywords <- list(
    keyword = c("SELECT", "FROM", "WHERE", "GROUP BY", "ORDER BY", "HAVING", "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", "OUTER JOIN", "ON", "AS", "AND", "OR", "NOT", "IS", "NULL", "IN", "BETWEEN", "EXISTS", "LIKE", "LIMIT", "DISTINCT"),
    func_ = c("COUNT", "SUM", "AVG", "MIN", "MAX"),
    operator = c("=", "<", ">", "<=", ">=", "<>", "!=")
  )
  
  sql_code <- gsub("<", "&lt;", sql_code)
  sql_code <- gsub(">", "&gt;", sql_code)
  
  # Replace SQL keywords with HTML span elements containing CSS classes
  for (k in names(keywords)) {
    for (w in keywords[[k]]) {
      sql_code <- gsub(paste0("\\b", w, "\\b"), paste0("<span class='", k, "'>", w, "</span>"), sql_code, ignore.case = TRUE)
    }
  }
  sql_code <- gsub("<span class='operator'><</span<span class='operator'>></span>","<",sql_code)
  
  html <- htmltools::HTML(sql_code)
  
  return(html)
}

