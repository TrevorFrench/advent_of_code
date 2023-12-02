# LIBRARIES
library(dplyr)

# READ INPUT DATA
calories <- read.delim('day_1/input.txt'
                       , sep = '\n'
                       , blank.lines.skip = FALSE
                       , header = FALSE)

# SPECIFY COLUMN NAME
colnames(calories)[1] <- "calories"

# GIVE EACH ELF A UNIQUE ID (ID ITERATES WHEN NA IS ENCOUNTERED)
elf_id = 1

for (i in 1:nrow(calories)) {
  if (is.na(calories[i, 'calories'])) {
    elf_id = elf_id + 1
  }
  calories[i, 'elf_id'] <- elf_id
}

# DROP NAs
calories <- na.omit(calories)

# AGGREGATE CALORIE DATA
agg_calories <- calories %>% 
                  group_by(elf_id) %>%
                  summarise(calories = sum(calories))

# PART I ANSWER
max(agg_calories$calories)

# ORDER BY CALORIES DESCENDING
top_calories <- sort(agg_calories$calories, decreasing = TRUE)

# PART II ANSWER
sum(top_calories[1:3])