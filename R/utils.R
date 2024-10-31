
library(stringr)

extract_fileids <- function(input_string){
  matches <- str_match_all(input_string, "([a-z]+)(\\d+)")[[1]]
  
  # Extract the names (prefixes) and numbers
  prefixes <- matches[, 2]
  numbers <- matches[, 3]
  
  # Create a named vector with numbers as strings
  named_vector <- setNames(as.character(numbers), prefixes)
  return(named_vector)
}

organize_filenames <- function(ff){
  df <- do.call(rbind,lapply(ff,extract_fileids))
  rownames(df) <- ff
  return(df)
}










 