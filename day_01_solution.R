
# Setup ========================================================================

# https://adventofcode.com/2023/day/1 by Eric Wastl

library(tidyverse)
library(glue)
library(assertthat)

# My preferred workflow is to keep workings in functions in a separate,
# source-on-save script, so I can rapidly tweak and prototype without cluttering
# the logical flow of the higher-level work - keep the essay tight, and the
# citations elsewhere. Kinda.

# I think this will also be useful for these kinds of coding challenges - where
# things end up needing epicycles, we can build in regression testing by forcing
# ourselves to tweak existing functions to support adaptations *while still*
# working for the earlier versions.

source("day_01_functions.R")

calibration_strings <- read_lines("data/day_01.txt")

# Part One =====================================================================

calibration_strings |>

  extract_calibration_digits() |>

  as.numeric() |>

  reduce(sum) |>

  print()

# 54597

# Part Two =====================================================================

# Now we need to add string representations of digits.

# Note that the text did *not* include zero...

# And let's retain *that* logic as-is, and just adapt the input instead with
# a new "digitify_strings" function.

# Hey presto:
calibration_strings |>

  digitify_strings() |>

  extract_calibration_digits() |>

  as.numeric() |>

  sum()

# 54039

# This was too low!

# Part 2.1 =====================================================================

# Uh oh: they gave us an example for this, which was:
#   xtwone3four
# Which they converted to:
#   2 4
# I.e.
#   x2ne34
# So the digits are decided by working left-to-right per string. Working through
# each digit tuple (1, one) doesn't work! I wondered what might happen if there
# was overlap...

const_search_string <- glue(
  "([:digit:]|{collapsed_str})",
  collapsed_str = paste(
    c(
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine"
    ),
    collapse = "|"
  )
)
# So we have a const that hits all digits and all English digits
# ([:digit:]|one|two|...)

# And basically all we need to do is *reverse the order of operations*!

calibration_strings |>

  extract_calibration_digits(search_string = const_search_string) |>

  digitify_strings() |>

  as.numeric() |>

  sum()

# 54506

# This was now too high!

## Inspecting our outputs: -----------------------

# I'm keeping this here as informative about how I go about troubleshooting
# what's going wrong:

testing_tibble <- tibble(

  index = 1:length(calibration_strings),

  version_1 = calibration_strings |>

    digitify_strings() |>

    extract_calibration_digits(),

  version_2 = calibration_strings |>

    extract_calibration_digits(search_string = const_search_string) |>

    digitify_strings(),

  string = calibration_strings,

  mismatch = version_1 != version_2

)

testing_tibble |> filter(mismatch) # |> View()

# Part 2.2 =====================================================================

# Hmmmm: we made an assumption above about the test case - that "twone" was
# getting treated as "two" "ne". But there's another interpretation consistent
# with the data we were actually presented: that in these cases, the characters
# get counted *for both possible digits* - e.g. "twone" becomes 2 1.

# I guess we can do this brute force by iterating through the individual
# characters in the string and "starting" from each of them. Not sure how to do
# standard regex versions without having them 'consume' the matches - while also
# avoiding absolutely disgusting lookarounds.

# Note also that we'll want to ensure we're checking for matches *that start at
# character i*, not just the next match after i, so we don't double-count
# anything:
#   twone => two
#    wone => one
#     one => one
#      ne => NA

# And what we'll do is retroactively tweak our extract_calibration_digits
# function to take a different search function...

calibration_strings |>

  extract_calibration_digits(
    search_string = const_search_string,
    search_function = extract_without_consuming
  ) |>

  digitify_strings() |>

  as.numeric() |>

  sum() |>

  print()

# 54504

## Benchmarking -------------------------------

# This now takes much longer to run - though still small in total because
# there's only 1000 strings.

### Version 1 -----------------

tictoc::tic()

calibration_strings |>

  digitify_strings() |>

  extract_calibration_digits() |>

  as.numeric() |>

  sum()

tictoc::toc() # 0.37 seconds for the original part 1 version

### Version 2 ----------------

tictoc::tic()

calibration_strings |>

  extract_calibration_digits(
    search_string = const_search_string,
    search_function = extract_without_consuming
  ) |>

  digitify_strings() |>

  as.numeric() |>

  sum() |>

  print()

tictoc::toc() # 3.95 seconds for the final version - ~10x slowdown!
