
# Setup ========================================================================

library(tidyverse)
library(glue)

source("day_06_functions.R")

raw_data <- read_lines("data/day_06.txt")

processed_data <- raw_data %>% tidy_day_06_data()


# Part 1 =======================================================================

all_strategies <- processed_data %>% expand_potential_strategies()


all_strategies %>%

  group_by(race) %>%

  summarise(winning_strategies = sum(beat_record)) %>%

  ungroup() %>%

  pull(winning_strategies) %>%

  prod() %>%

  print()

# Part 2 =======================================================================

# Lol - I rather guessed there'd be some wrinkle to prevent you from brute
# forcing it!

actual_data <- processed_data %>% reinterpret_day_06_data()

actual_data %>% pinpoint_winning_strategies() %>% View()
