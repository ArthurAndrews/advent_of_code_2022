library(tidyverse)

# priorities
item <- tibble(
  contents = c(letters, toupper(letters)),
  priority = 1:52
)

# store sack as a long data frame
sack <- read_delim("3.dat", col_names = "contents", delim = "\n") %>%
  mutate(
    id = row_number(), 
    contents = str_split(contents, "")
  ) %>%
  unnest(contents) %>%
  # assign compartment
  group_by(id) %>%
  mutate(
    location = row_number(),
    compartment = if_else(location <= (max(location) / 2), 1, 2)
  ) %>%
  ungroup() %>%
  relocate(id) 

# solution 1
sack_common <- sack %>%
  group_by(id) %>%
  summarize(common = intersect(contents[compartment == 1], contents[compartment == 2])) %>%
  left_join(item, by = c("common" = "contents"))

sack_common$priority %>% sum()  

# solution 2
sack <- sack %>%
  mutate(grp = ceiling(id / 3)) 

sack_badge <- sack %>%
  group_by(grp) %>%
  summarize(badge = split(contents, id) %>% reduce(intersect)) %>%
  left_join(item, by = c("badge" = "contents"))

sack_badge$priority %>% sum()

# plot
library(ggfx)
sack %>%
  group_by(id) %>%
  mutate(
    common = contents == intersect(contents[compartment == 1], contents[compartment == 2]),
    contents_match = if_else(common, contents, NA_character_),
    contents = factor(contents, item$contents),
  ) %>%
  filter(id <= 50) %>%
  ggplot() + 
  geom_raster(aes(contents, id, fill = common)) +
  with_outer_glow(
    geom_text(aes(contents, id, label = contents_match), na.rm = TRUE, size = 3, color = "cornflowerblue"),
    colour = "lightblue",
    #sigma = 12
    expand = 5
  ) +
  facet_wrap(~compartment, nrow = 1, labeller = label_both) +
  theme_dark() + 
  scale_y_continuous(breaks = 1:50) +
  ylab("elf id") 

             