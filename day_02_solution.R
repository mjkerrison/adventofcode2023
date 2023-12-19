
# Setup ========================================================================

library(tidyverse)
library(glue)

source("day_02_functions.R")

raw_data <- read_lines("data/day_02.txt")

# Testing our reading function:
raw_data %>%
  head(1) %>%
  clean_day_02_data()

# Looks good!

processed_data <- raw_data %>% clean_day_02_data()

# Part 1 =======================================================================

# So we basically just need to filter games based on our constraints on the
# maxes of individual cube colours.

restrict_games_by_colour(
  processed_data,
  # Constraints:
  all(red <= 12),
  all(green <= 13),
  all(blue <= 14)
) %>%

  distinct(game_id) %>%

  pull() %>%

  sum()

# Part 2 =======================================================================

# For this part we need to find the smallest number of cubes that a game could
# have been played with, then do some maths on that list of numbers.

crunch_game_requirements(processed_data) %>%

  pull(game_power) %>%

  sum()
