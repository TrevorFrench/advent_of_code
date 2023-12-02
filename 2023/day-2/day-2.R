# PART I
ledger <- readr::read_file('2023/day-2/input.txt')
split_ledger <- strsplit(ledger,"\r\n")

id <- c()
data <- c()
for (game in split_ledger[[1]]) {
  game_id <- stringr::str_extract(game, "Game (\\d+):", 1)
  game_data <- gsub("Game (\\d+): ", "", game)
  for (data_point in stringr::str_split(game_data, ";")[[1]]) {
    id <- append(id, game_id)
    data <- append(data, data_point)
  }
}

df <- data.frame(game_id = as.integer(id), round_data = data)

df <- df |> 
  dplyr::mutate(red = stringr::str_extract(round_data, "(\\d+) red", 1)) |>
  dplyr::mutate(green = stringr::str_extract(round_data, "(\\d+) green", 1)) |>
  dplyr::mutate(blue = stringr::str_extract(round_data, "(\\d+) blue", 1))
df$red <- as.integer(df$red)
df$green <- as.integer(df$green)
df$blue <- as.integer(df$blue)
# Replace NAs with 0s
df[is.na(df)] <- 0
# 12 red cubes, 13 green cubes, and 14 blue cubes
ruled_out <- df |>
  dplyr::filter(red > 12 | green > 13 | blue > 14)

games <- unique(df$game_id)
impossible <- unique(ruled_out$game_id)
possible <- setdiff(games, impossible)

sum(as.integer(possible))

# PART II
ledger <- readr::read_file('2023/day-2/input.txt')
split_ledger <- strsplit(ledger,"\r\n")

id <- c()
data <- c()
for (game in split_ledger[[1]]) {
  game_id <- stringr::str_extract(game, "Game (\\d+):", 1)
  game_data <- gsub("Game (\\d+): ", "", game)
  for (data_point in stringr::str_split(game_data, ";")[[1]]) {
    id <- append(id, game_id)
    data <- append(data, data_point)
  }
}

df <- data.frame(game_id = as.integer(id), round_data = data)

df <- df |> 
  dplyr::mutate(red = stringr::str_extract(round_data, "(\\d+) red", 1)) |>
  dplyr::mutate(green = stringr::str_extract(round_data, "(\\d+) green", 1)) |>
  dplyr::mutate(blue = stringr::str_extract(round_data, "(\\d+) blue", 1))
df$red <- as.integer(df$red)
df$green <- as.integer(df$green)
df$blue <- as.integer(df$blue)
# Replace NAs with 0s
df[is.na(df)] <- 0

fewest <- df |> dplyr::group_by(game_id)
fewest <- fewest |> dplyr::summarise(red = max(red), green = max(green), blue = max(blue))
fewest <- fewest |> dplyr::mutate(power = red * green * blue)
sum(fewest$power)
