# Setup ========================================================================

library(tidyverse)
library(glue)

source("day_05_functions.R")

raw_data <- read_raw_day_05_data("data/day_05.txt")

# Wrangling data ===============================================================

tbl_seeds <- fetch_seed_table(raw_data)

dimensional_links <- list(

  c("seed", "soil"),

  c("soil", "fertilizer"),

  c("fertilizer", "water"),

  c("water", "light"),

  c("light", "temperature"),

  c("temperature", "humidity"),

  c("humidity", "location")

)

all_maps <- set_names(

  dimensional_links,

  dimensional_links |>
    map(\(x) paste(x, collapse = "_to_"))

) |>

  map(function(x){

    return(
      list(
        from = x[1],
        to = x[2],
        map = fetch_input_map(raw_data,
                              x[1],
                              x[2])
      )
    )

  })

# Solving Part 1 ===============================================================

# So you could solve this by just converting things into an "offset" as
# everything is neatly one-to-one, but in some different range. I feel like this
# might get tripped up by some later twist, so I'm not going to do this... at
# the cost of some complexity.


everything_joined <- join_everything(tbl_seeds,
                                     all_maps)

everything_joined |>
  pull(location) |>
  min(na.rm = TRUE) |>
  print()

# Solving Part 2 ===============================================================

# So now we pivot to seeds actually also defining ranges.
# Easy enough: we can bound this - take the largest and smallest seed values in
# a range, and join those to everything. Regardless of how the (one-to-one) m
# mapping occurs, because everything's always just sequential, one of the seed
# bounds will produce the highest or lowest ultimate value.

# Wait - no, that's not right... There's no guarantees that a range of seed
# values will produce contiguous results. You could totally have pathological
# cases where actually just a seed value in the middle is the one that is
# cherry-picked to map to a much "better" target value.

# TL;DR I think the approach here is backward induction, figuring out the "real"
# ranges, i.e.:
#   - Take humidity<>location.
#   - Figure out the minimal set of humidity values that fully identify
#     locations - i.e., what are all the (min, max) pairs of humidities that hit
#     all the ranges of locations?
#   - These then create our "effective" humidity ranges, so we carry *those*
#     forward.
#       - Probably with some extra ranges included for the gaps, in which values
#         get mapped to themselves.
#   - Repeat the process with temperature<>humidity, then all the way up the
#     chain.
#   - By the end, we should have a manageable number of (min, max) points to
#     check; we just need to find which seed value falls closest to the bottom
#     of the lowest-ultimate-location soil range.

# So yeah, the trick is a combo of the tools:
#   - Backward induction
#   - Extrapolating the full search space from the space given
#   - Converting given ranges into ranges we care about

# This still sounds like a real pain to implement - you need to calculate gaps
# and overlaps, with theoretically no upper bound (i.e., [0, inf) ). And to be
# honest you *could* have negative values with some of these dimensions (e.g.
# temperature) - but it looks like all values are positive (maybe because
# they're actually IDs - which I don't think is super explicit from the text.)

tbl_seeds_v2 <- fetch_seed_table_v2(raw_data)

everything_joined_v2 <- join_everything(tbl_seeds_v2,
                                        all_maps)

everything_joined_v2 |>
  pull(location) |>
  min(na.rm = TRUE) |>
  print()

# Currently yields same result as for part 1.
