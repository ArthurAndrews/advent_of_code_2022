library(tidyverse)
library(magrittr)

# define inputs ----

## codes
rps <- c("r", "p", "s")

rps_opponent <- tibble(
  opponent = rps,
  opponent_code = c("A", "B", "C"),
)

rps_self <- tibble(
  self = rps,
  self_code = c("X", "Y", "Z"),
  score1 = c(1, 2, 3)
)

## game result
game <- matrix("draw", nrow = 3, ncol = 3) %>% 
  set_colnames(rps) %>%
  set_rownames(rps)

game["r", "p"] <- "win"
game["r", "s"] <- "loss"
game["p", "r"] <- "loss"
game["p", "s"] <- "win"
game["s", "r"] <- "win"
game["s", "p"] <- "loss"

game <- game %>%
  as_tibble(rownames = "opponent") %>%
  pivot_longer(-opponent, names_to = "self", values_to = "outcome")

## outcome score
outcome <- tibble(
  outcome = c("win", "draw", "loss"),
  score2 = c(6, 3, 0)
)

# function to play a tournament ----

play <- function(input) {
  result <- input %>%
    left_join(rps_opponent, by = "opponent_code") %>%
    left_join(rps_self, by = "self_code") %>%
    left_join(game, by = c("opponent", "self")) %>%
    left_join(outcome, by = "outcome") %>%
    mutate(score = score1 + score2)
  return(lst(score = sum(result$score), result))
}

# test input ----
input <- tibble(
  opponent_code = c("A", "B", "C"),
  self_code = c("Y", "X", "Z")
)

input %>% play()

# round 1 ----

round1 <- read_delim("data//2.dat", col_names = c("opponent_code", "self_code"))
round1 %>% play()

# round 2 ----

new_guide <- tibble(
  outcome_code = c("X", "Y", "Z"),
  outcome = c("loss", "draw", "win")
)

round2 <- round1 %>% rename(outcome_code = self_code)

play2 <- function(input) {
  result <- input %>%
    left_join(rps_opponent, by = "opponent_code") %>%
    left_join(new_guide, by = "outcome_code") %>%
    left_join(game, by = c("opponent", "outcome")) %>%
    left_join(outcome, by = "outcome") %>%
    left_join(rps_self, by = "self") %>%
    mutate(score = score1 + score2)
  return(lst(score = sum(result$score), result))
}

input <- round2
round2 %>% play2()
