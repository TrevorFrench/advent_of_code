library(tokenizers)
library(stopwords)
library(dplyr)
library(readr)

read_files <- function(input) {
  setwd(input)
  files <- list.files(pattern="*.txt")
  df <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(df) <- c('tokens', 'values')
  for (file in files) {
    corpus <- read_file(file)
    tokens <- tokenize_words(corpus, stopwords = stopwords("en"))
    token_table <- table(tokens)
    list_temp <- c()
    list_temp[[ file ]] <- token_table
    df1 <- data.frame(list_temp)
    colnames(df1) <- c('tokens', file)
    df <- merge(x = df, y = df1, by = "tokens", all = TRUE)
  }
  return(df)
}

index <- read_files('~/corpus')

search_files <- function(query, index) {
  df <- index[index$tokens == query, ]
  return(df[1,])
}

search <- search_files('test1', index)

xy.list <- setNames(split(search, seq(nrow(search))), rownames(search))