
# Setup ========================================================================

library(tidyverse)
library(glue)

source("day_03_functions.R")

raw_data <- read_lines("data/day_03.txt")

# Just check what we're dealing with character-wise:
raw_data |>
  paste0() |>
  str_split(pattern = "") |>
  unlist() |>
  unique()


"123..54$19" |> str_split("[^0-9]")

raw_data |>
  head(10) |>

  find_numbers() |>

  # head(1) |>
  #
  # pull(endpoints) |>

  identity()


# Not yet complete!
