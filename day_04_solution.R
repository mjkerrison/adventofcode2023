
# Setup ========================================================================

library(tidyverse)
library(glue)

source("day_04_functions.R")

raw_data <- read_lines("data/day_04.txt")

processed_data <- raw_data %>%

  process_day_04_data()

# Part 1 =======================================================================

processed_data %>%
  map(function(x){pluck(x, "value")}) %>%
  unlist() %>%
  sum() %>%
  print()

# Part 2 =======================================================================

id_wins_table <- extract_card_attribute_tbl(processed_data,
                                            "n_wins")

id_value_table <- extract_card_attribute_tbl(processed_data,
                                             "value")


find_card_copies(id_wins_table)

# Well, we got Part 1 right anyway...

# Will park this and return.
