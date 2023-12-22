
# Setup ========================================================================

library(tidyverse)
library(glue)
library(assertthat)

source("day_08_functions.R")

raw_data <- read_lines("data/day_08.txt")

instrx <- raw_data %>% read_day_08_instrx()

maps <- raw_data %>% read_day_08_maps()


# Part 1 =======================================================================

navigate(starting_map = "AAA",
         instrx,
         maps)

# Part 2 =======================================================================

# Backtest our new thing on the simple case:

navigate_simultaneously(starting_maps = c("AAA"),
                        done_criteria = "^ZZZ$",
                        instrx,
                        maps)

# And this was actually faster than the original version! No doubt because we're
# now doing this in like O(1) time or something because we're going directly to
# the relevant rows - the maps object acts more like a dict.

navigate_simultaneously(

  starting_maps = maps |> filter(str_ends(from_map, "A")) |> pull(from_map),

  done_criteria = "Z$",

  instrx,
  maps,

  debug = TRUE,
  debug_step_interval = 100000

)

# Tapped out of the brute force approach after like an hour? And 224,300,000
# iterations...

# Ah, looking at the reddit - of course this is a Lowest Common Multiple
# problem.
