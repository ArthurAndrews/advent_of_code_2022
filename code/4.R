library(tidyverse)

# read file and parse sequences to a long data frame
clean <- "data//4.txt" %>%
  read_csv(col_names = c("elf1", "elf2")) %>%
  mutate(pair = row_number(), .before = 1) %>%
  pivot_longer(c(elf1, elf2), names_to = "elf", values_to = "sequence_chr") %>%
  mutate(
    elf1 = elf == "elf1",
    elf2 = elf == "elf2",
  ) %>% 
  separate(sequence_chr, into = c("from", "to"), sep = "-") %>%
  mutate(sequence = map2(from, to, seq)) %>%
  unnest(sequence) 

# identify overlapping sequences
clean_pairs <- clean %>%
  group_by(pair) %>%
  summarize(
    overlap = all(sequence[elf1] %in% sequence[elf2]) |
      all(sequence[elf2] %in% sequence[elf1]),
    any_overlap = !is_empty(intersect(sequence[elf1], sequence[elf2]))
  )

# solution 1
sum(clean_pairs$overlap) %>% print()

# solution 2
sum(clean_pairs$any_overlap) %>% print()
