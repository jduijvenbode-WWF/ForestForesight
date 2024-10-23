config <- config_load()

get_feature_metadata=function(){
  library(utils)

  # URL of the Markdown file
  url <- config$URL_MARKDOWN

  # Read the Markdown file
  markdown_text <- readLines(url)
  markdown_text <- gsub("#", "%23", markdown_text)
  # Extract the table from the Markdown text
  table_start <- grep("\\|feature", markdown_text)
  table_end <- grep("\\[\\^1\\]overview", markdown_text) - 1
  table_text <- markdown_text[(table_start):(table_end)]

  # Read the table into a data frame
  df <- read.table(text = table_text, header = TRUE, sep = "|", fill = TRUE, stringsAsFactors = FALSE)
  df$source <- gsub("%23","#",df$source)

  # Clean up column names
  colnames(df) <- trimws(gsub("\\|", "", colnames(df)))
  return(df)
}
