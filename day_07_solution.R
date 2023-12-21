
# Setup ========================================================================

library(tidyverse)
library(glue)
library(assertthat)

source("day_07_functions.R")

raw_data <- read_lines("data/day_07.txt")

processed_data <- raw_data |> read_07_data()



# Part 1 =======================================================================

# Lexical ordering! Makes comparisons by best, then 2nd best, then ... much
# easier...

const_card_indices <- tibble::tribble(
  ~high_card, ~card_sort_index,
         "A",              "m",
         "K",              "l",
         "Q",              "k",
         "J",              "j",
         "T",              "i",
         "9",              "h",
         "8",              "g",
         "7",              "f",
         "6",              "e",
         "5",              "d",
         "4",              "c",
         "3",              "b",
         "2",              "a"
)

const_hand_indices <- tibble::tribble(
        ~hand_label, ~hand_sort_index,
   "Five of a kind",              "g",
   "Four of a kind",              "f",
       "Full house",              "e",
  "Three of a kind",              "d",
         "Two pair",              "c",
         "One pair",              "b",
        "High card",              "a"
)



bets <- processed_data


hand_labels <- processed_data |>

  expand_hands() |>

  identify_hand_label(const_hand_indices) |>

  select(hand_id, hand_sort_index)


high_cards <- processed_data |>

  expand_hands() |>

  get_high_card(const_card_indices)


# OK - no problem. We just rank the hands and tally it up, right?

rank_hands(bets,
           hand_labels,
           high_cards,
           rank_method = "first") |> #View()

  pull(winnings) |>

  sum() |>

  print()

# 245013480

# Uh oh - this was too low. Looking at the first few, we have some hands that
# rank the same, but have very different bets, such that x*998 and x*997 makes
# quite a difference. Is this a ranking-method problem? This is unspecified in
# the text for the challenge...

rank_hands(bets,
           hand_labels,
           high_cards,
           rank_method = "max") |> #View()

  pull(winnings) |>

  sum() |>

  print()

# 245031672

# Nope - this was also wrong - and still too low!
# Ah - it could be that we need *dense* max ranking - so e.g. 999 - 999 - 998,
# rather than 999 - 999 - 997.

rank_hands(bets,
           hand_labels,
           high_cards,
           rank_method = "dense") |>

  pull(winnings) |>

  sum() |>

  print()

# 236908713

# But this is lower - and of course it is, because we're shrinking how many
# ranks we give out. We don't give out 1000 any more! So similarly, "min"
# ranking would also be lower, so we don't need to worry about that.

# Part 1 Redux =================================================================

# Ah - our issue is that we were comparing high cards by their strength, not
# by their position!

rank_hands(

  bets = processed_data,

  hands = processed_data |>
    expand_hands() |>
    identify_hand_label(),

  cards = processed_data |>
    expand_hands() |>
    get_high_card(by = "position"),

  rank_method = "first"

) |>

  pull(winnings) |>

  sum() |>

  print()

# 245820565 - higher, but still wrong!


get_all_possible_results(processed_data) |> print()

# Not first
# ...
# ...
# Not dense

# And now I'm on a 10 minute timeout :(

# Troubleshooting ==============================================================

test_on <- function(path_to_unit_test,
                    hand_dict,
                    card_dict,
                    is_part_2 = FALSE){


  test_data <- read_lines(path_to_unit_test) |>

    read_07_data()


  rank_hands(

    test_data,

    test_data |>
      expand_hands() |>
      identify_hand_label(
        hand_dict,
        is_part_2
      ),

    test_data |>
      expand_hands() |>
      get_high_card(
        card_dict,
        by = "position"
      ),

    rank_method = "first"

  )

}

# From the text
test_on("data/day_07_test.txt",
        const_hand_indices,
        const_card_indices,
        is_part_2 = FALSE)


# From user LxsterGames
# @ https://www.reddit.com/r/adventofcode/comments/18cr4xr/2023_day_7_better_example_input_not_a_spoiler/
test_on("data/day_07_test_reddit.txt",
        const_hand_indices,
        const_card_indices,
        is_part_2 = FALSE)
# Target:
#   Pt 1 = 6592
#   Pt 2 = 6839


# Oh, There It Is ==============================================================

rank_hands(

  bets = processed_data,

  hands = processed_data |>
    expand_hands() |>
    identify_hand_label(),

  cards = processed_data |>
    expand_hands() |>
    get_high_card(by = "position"),

  rank_method = "dense"

) |> group_by(rank) |> filter(n() > 1) |> ungroup()


# Bloody hell...
#
# So it turns out I had a typo in my card lookup: both J and T were being given
# "j", rather than "i" and "j" respectively.
#
# The wording of the text subtly indicates that there are no exactly-matching
# hands, so it turns out that "first" and "dense" ranking should give the same
# results.
#
# I only spotted this (via code above, which should now return no rows) after
# checking the reddit a bunch and not finding anything, including running
# someone's extra test cases and getting the right result!!!


get_all_possible_results(processed_data) |> print()
# Look at that - they now all get the same result.

# And this won't quite be reproducible - the above code should basically all
# give the right output now. Lol.


# Part 2 =======================================================================


const_card_indices_pt2 <- tibble::tribble(
  ~high_card, ~card_sort_index,
         "A",              "m",
         "K",              "l",
         "Q",              "k",
         "T",              "i",
         "9",              "h",
         "8",              "g",
         "7",              "f",
         "6",              "e",
         "5",              "d",
         "4",              "c",
         "3",              "b",
         "2",              "a",
         "J",              "Z" # review your ASCII tables
)


rank_hands(

  bets = processed_data,

  hands = processed_data |>
    expand_hands() |>
    identify_hand_label(
      hand_dict = const_hand_indices,
      is_part_2 = TRUE
    ),

  cards = processed_data |>
    expand_hands() |>
    get_high_card(
      const_card_indices_pt2,
      by = "position"
    ),

  rank_method = "dense"

) |>

  pull(winnings) |>

  sum()

# 247967442 - too high!

# Ah - we weren't dealing with Js right.

# So instead, we'll move to less "if" more matrices...

# And now: 248469489

# But that'll also be too high hmmmm

# And after some tweaking and testing on the reddit data:
# 247899149
