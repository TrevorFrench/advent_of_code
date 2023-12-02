# PART I
corpus <- readr::read_file('2023/input.txt')
tokens <- tokenizers::tokenize_words(corpus)[[1]]
decode <- function(tkn) {
  num_tkn <- stringr::str_extract_all(tkn, "\\d")[[1]]
  as.integer(paste0(num_tkn[1], num_tkn[length(num_tkn)]))
}
sum(sapply(tokens, decode))

# PART II
replacements <- c('1','2','3','4','5','6','7','8','9')
pattern <- c('one','two','three','four','five','six','seven','eight','nine')
names(replacements) <- pattern
search <- append(replacements, pattern)
corpus <- readr::read_file('2023/input.txt')
tokens <- tokenizers::tokenize_words(corpus)[[1]]

decode <- function(tkn) {
  tkn <- gsub('eightwo','eighttwo', tkn)
  tkn <- gsub('eighthree','eightthree', tkn)
  tkn <- gsub('sevenine','sevennine', tkn)
  tkn <- gsub('oneight','oneeight', tkn)
  tkn <- gsub('twone','twoone', tkn)
  tkn <- gsub('threeight','threeeight', tkn)
  tkn <- gsub('fiveight','fiveeight', tkn)
  tkn <- gsub('nineight','nineeight', tkn)
  extract <<- unlist(stringr::str_extract_all(tkn, paste(search, collapse = "|")))
  nums <<- stringr::str_replace_all(extract, replacements)
  as.integer(paste0(nums[1], nums[length(nums)]))
}
sum(sapply(tokens, decode))

#53868
