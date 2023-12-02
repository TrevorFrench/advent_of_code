# LIBRARIES
library(dplyr)

# READ INPUT DATA
strategy <- read.delim('day_2/input.txt'
                       , sep = ''
                       , blank.lines.skip = FALSE
                       , header = FALSE)

# SPECIFY COLUMN NAME
colnames(strategy) <- c('opponent', 'me')

# DICTIONARIES
decrypt <- c('X' = 'A', 'Y' = 'B', 'Z' = 'C')
choice_points <- c('A' = 1, 'B' = 2, 'C' = 3)
outcome_points <- c('win' = 6, 'draw' = 3, 'loss' = 0)
win <- c('A' = 'C', 'B' = 'A', 'C' = 'B')

# DETERMINE POINTS
for (i in 1:nrow(strategy)) {
  opponent <- strategy[i, 'opponent']
  me <- decrypt[strategy[i, 'me']]
  if (opponent == me) {
    outcome <- 'draw'
  } else if (win[me] == opponent) {
    outcome <- 'win'
  } else {
    outcome <- 'loss'
  }
  strategy[i, 'choice_points'] <- choice_points[[me]]
  strategy[i, 'outcome_points'] <- outcome_points[[outcome]]
}

strategy <- strategy %>%
  mutate('total_points' = choice_points + outcome_points)

# PART I ANSWER
sum(strategy$total_points)

# READ INPUT DATA
true_strategy <- read.delim('day_2/input.txt'
                       , sep = ''
                       , blank.lines.skip = FALSE
                       , header = FALSE)

# SPECIFY COLUMN NAME
colnames(true_strategy) <- c('opponent', 'me')

# TRUE DECRYPTION DICTIONARY
true_decrypt = c('X' = 'loss', 'Y' = 'draw', 'Z' = 'win')

# DETERMINE CHOICE & POINTS
for (i in 1:nrow(true_strategy)) {
  opponent <- true_strategy[i, 'opponent']
  rig <- true_decrypt[true_strategy[i, 'me']]
  
  if (rig == 'loss') {
    choice <- win[opponent]
  } else if (rig == 'win') {
    choice <- names(win[which(win==opponent)])
  } else {
    choice <- opponent
  }
  
  true_strategy[i, 'choice_points'] <- choice_points[[choice]]
  true_strategy[i, 'outcome_points'] <- outcome_points[[rig]]
}

true_strategy <- true_strategy %>%
  mutate('total_points' = choice_points + outcome_points)

# PART II ANSWER
sum(true_strategy$total_points)
