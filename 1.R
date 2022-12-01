library(tidyverse)

# read file
food <- "1.dat" %>%
  read_delim(delim = "\n", col_names = "calories", skip_empty_rows = FALSE) %>%
  # create column for elf
  mutate(elf = 1 + cumsum(is.na(calories))) %>%
  drop_na(calories)

# sum calories by elf
food_summary <- food %>%
  group_by(elf) %>%
  summarize(across(calories, sum)) %>%
  arrange(-calories) 

# challenge 1
food_summary$calories[1]

# challenge 2
food_summary$calories[1:3] %>% sum()
