# PART I
schematic <- readr::read_file('2023/day-3/input.txt')
split_schematic <- strsplit(schematic,"\r\n")

id <- 1
ids <- c()
start <- c()
end <- c()
type <- c()
numbers <- c()
for (line in split_schematic[[1]]) {
  nums <- stringr::str_locate_all(line,"\\d+")
  if (nrow(nums[[1]]) > 0) {
    for (row in 1:nrow(nums[[1]])) {
      ids <- append(ids, id)
      start <- append(start, nums[[1]][row,'start'])
      end <- append(end, nums[[1]][row,'end'])
      numbers <- append(numbers, stringr::str_sub(line, nums[[1]][row,'start'], nums[[1]][row,'end']))
      type <- append(type, 'number')
    }
  }
  
  syms <- stringr::str_locate_all(line,"[^a-zA-Z0-9.]+")
  if (nrow(syms[[1]]) > 0) {
    for (row in 1:nrow(syms[[1]])) {
      ids <- append(ids, id)
      start <- append(start, syms[[1]][row,'start'])
      end <- append(end, syms[[1]][row,'end'])
      numbers <- append(numbers, 'NULL')
      type <- append(type, 'symbol')
    }
  }
  id <- id + 1
}

df <- data.frame(id = ids, start = start, end = end, number = numbers, type = type)
nums <- df |> dplyr::filter(type == 'number')
syms <- df |> dplyr::filter(type == 'symbol')

matches <- c()
for (row in 1: nrow(nums)) {
  row_id <- nums[row, 'id']
  row_start <- nums[row, 'start']
  row_end <- nums[row, 'end']
  temp <- syms |> dplyr::filter(id >= row_id - 1 & id <= row_id + 1)
  temp <- temp |> dplyr::filter(start >= row_start - 1 & end <= row_end + 1)
  if (nrow(temp) > 0) {match <- 1} else {match <- 0}
  matches <- append(matches, match)
}

nums$matches <- matches
nums$number <- as.integer(nums$number)
sum(nums[nums$matches == 1, "number"])

# PART II
schematic <- readr::read_file('2023/day-3/input.txt')
split_schematic <- strsplit(schematic,"\r\n")

id <- 1
ids <- c()
start <- c()
end <- c()
type <- c()
numbers <- c()
for (line in split_schematic[[1]]) {
  nums <- stringr::str_locate_all(line,"\\d+")
  if (nrow(nums[[1]]) > 0) {
    for (row in 1:nrow(nums[[1]])) {
      ids <- append(ids, id)
      start <- append(start, nums[[1]][row,'start'])
      end <- append(end, nums[[1]][row,'end'])
      numbers <- append(numbers, stringr::str_sub(line, nums[[1]][row,'start'], nums[[1]][row,'end']))
      type <- append(type, 'number')
    }
  }
  
  syms <- stringr::str_locate_all(line,"\\*")
  if (nrow(syms[[1]]) > 0) {
    for (row in 1:nrow(syms[[1]])) {
      ids <- append(ids, id)
      start <- append(start, syms[[1]][row,'start'])
      end <- append(end, syms[[1]][row,'end'])
      numbers <- append(numbers, 'NULL')
      type <- append(type, 'symbol')
    }
  }
  id <- id + 1
}

df <- data.frame(id = ids, start = start, end = end, number = numbers, type = type)
nums <- df |> dplyr::filter(type == 'number')
syms <- df |> dplyr::filter(type == 'symbol')

matches <- c()
num_1 <- c()
num_2 <- c()
for (row in 1: nrow(syms)) {
  row_id <- syms[row, 'id']
  row_start <- syms[row, 'start']
  row_end <- syms[row, 'end']
  temp <- nums |> dplyr::filter(id >= row_id - 1 & id <= row_id + 1)
  temp <- temp |> dplyr::filter((start >= row_start - 1 & start <= row_end + 1) | (end >= row_start - 1 & end <= row_end + 1))
  if (nrow(temp) == 2) {
    match <- 1
    num_1 <- append(num_1, temp[1, 'number'])
    num_2 <- append(num_2, temp[2, 'number'])
  } else {
      match <- 0
      num_1 <- append(num_1, 0)
      num_2 <- append(num_2, 0)
  }
  matches <- append(matches, match)
}

syms$matches <- matches
syms$num_1 <- as.integer(num_1)
syms$num_2 <- as.integer(num_2)
syms <- syms |> dplyr::mutate(gear_ratio = num_1 * num_2)
sum(syms$gear_ratio)

#sum(nums[nums$matches == 1, "number"])