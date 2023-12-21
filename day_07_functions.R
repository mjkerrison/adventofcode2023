

read_07_data <- function(raw_lines){

  tibble(

    hand = raw_lines |>
      str_extract("^.{5}") |>
      unlist(),

    bet = raw_lines |>
      str_extract("[:digit:]+?$") |>
      unlist() |>
      as.numeric()

  ) |>

    mutate(hand_id = 1:n(), .before = 1)

}


expand_hands <- function(hand_data){

  hand_data |>

    mutate(individual_cards = str_split(hand, "")) |>

    unnest(individual_cards)

}


identify_hand_label <- function(expanded_hand_data,
                                hand_dict = const_hand_indices,
                                is_part_2 = FALSE){


  calculate_effective_counts <- function(card_labels, counts){


    # Start with the counts, optionally zero out the Js, and resort to push
    # any now-zero-formerly-J slots to the end.
    pre_joker_counts <- sort(
      counts * (1 - (card_labels == "J") * is_part_2),
      decreasing = TRUE
    )

    # print(pre_joker_counts)

    # Figure out a J-based addition for the first position - this is the core
    # of the logic. I'm pretty sure it just works to add the Js to the first
    # position of the sorted tallies - for all hands!
    joker_addition <- sort(
      counts * (1 - (card_labels != "J")) * is_part_2,
      decreasing = TRUE
    )

    # print(joker_addition)

    effective_counts <- pre_joker_counts + joker_addition

    #
    # # Start with the counts - either everything ex Js, or everything for pt1
    # effective_counts <- counts[which(card_labels != "J" | !is_part_2)] +
    #
    #   (-1 * counts[which(card_labels == "J")])
    #
    #   # Remove the J component if we're in part 2
    #   (counts * (card_labels == "J") * is_part_2) +
    #
    #   # Rearrange the J component to the 1st position and add em, iff part 2
    #   sort(counts * (card_labels == "J") * is_part_2, decreasing = TRUE)
    #
    # # Re-sort them to push any zeroes (where the Js originally were) to the end.
    #
    # sort(effective_counts, decreasing = TRUE)

    return(effective_counts)

  }


  results <- expanded_hand_data |>

    group_by(hand_id, individual_cards) |>

    summarise(count = n()) |>

    ungroup() |>

    arrange(hand_id, desc(count)) |>

    group_by(hand_id) |>

    summarise(

      frequencies = paste(
        calculate_effective_counts(individual_cards, count),
        collapse = "-"
      )

    ) |>

    ungroup() |>

    mutate(

      hand_label = case_when(

        # Edge case logic for how we're checking Part 2:
        str_starts(frequencies, "6")   ~ "Five of a kind",

        str_starts(frequencies, "5")   ~ "Five of a kind",
        str_starts(frequencies, "4")   ~ "Four of a kind",
        str_starts(frequencies, "3-2") ~ "Full house",
        str_starts(frequencies, "3")   ~ "Three of a kind",
        str_starts(frequencies, "2-2") ~ "Two pair",
        str_starts(frequencies, "2")   ~ "One pair",
        TRUE                           ~ "High card"

      )

    ) |>

    left_join(
      hand_dict,
      by = "hand_label"
    )

  assert_that(
    nrow(results) == length(unique(results$hand_id)),
    msg = "Did not uniquely identify hands!"
  )

  return(results)


}


get_high_card <- function(expanded_hand_data,
                          card_index_dict = const_card_indices,
                          by = c("strength", "position")){

  # Note - we do NOT sort by card_sort_index! We don't compare 'highest card'
  # vs 'highest card', 'second-highest' vs 'second-highest' ... - we literally
  # just do it by position. So first to first, second to second, etc.

  # Whoops! We'll keep the mistake in by building around it - so our results
  # still tell the go-to-woe-to-done narrative.

  by <- match.arg(by)

  optionally_arrange_by_strength <- function(x){

    if(by == "strength"){

      x |> arrange(hand_id, desc(card_sort_index))

    } else {

      x

    }

  }


  results <- expanded_hand_data |>

    left_join(card_index_dict,
              by = c("individual_cards" = "high_card"),
              keep = TRUE) |>

    optionally_arrange_by_strength() |>

    group_by(hand_id) |>

    summarise(card_sorting = paste(card_sort_index, collapse = "")) |>

    ungroup()


  assert_that(
    nrow(results) == length(unique(results$hand_id)),
    msg = "Did not uniquely identify hands!"
  )

  return(results)

}


rank_hands <- function(bets,
                       hands,
                       cards,
                       rank_method = c("first",
                                       "max",
                                       "min",
                                       "dense")){

  rank_first <- function(x){

    x |>

      # We also sort by hand_id so that "first" as rank_method does something
      # that makes vague sense... This won't impact rank()ing by max or min.
      arrange(desc(final_rank), hand_id) |>

      mutate(rank = n():1)

  }

  rank_max <- function(x){

    x |>

      mutate(rank = rank(final_rank, ties.method = "max"))

  }

  rank_min <- function(x){

    x |>

      mutate(rank = rank(final_rank, ties.method = "min"))

  }

  # Could implement a short thing here for the other rank() options - but
  # they're not going to be of interest (presumably)

  rank_dense <- function(x){

    # No sorting necessary, as dplyr::min_rank and dense_rank do golf scoring

    x |>

      mutate(
        rank = dplyr::dense_rank(final_rank)
      )

  }


  rank_fn_selected <- switch(rank_method,
                             first = rank_first,
                             max = rank_max,
                             min = rank_min,
                             dense = rank_dense)


  bets |>

    left_join(hands, by = "hand_id") |>

    left_join(cards, by = "hand_id") |>

    mutate(final_rank = paste0(hand_sort_index, card_sorting)) |>

    # Note that I have pushed sorting into the ranking functions so it's clear
    # what (if any) sorting each one actually relies on.

    rank_fn_selected() |>

    arrange(desc(rank)) |>

    mutate(winnings = rank * bet)

}



get_all_possible_results <- function(processed_data){

  tibble(

    by = c("position"),

    methods = c("first", "max", "min", "dense"),

    results = pmap_dbl(list(by, methods), function(by_i, method_i){

      rank_hands(

        bets = processed_data,

        hands = processed_data |>
          expand_hands() |>
          identify_hand_label(),

        cards = processed_data |>
          expand_hands() |>
          get_high_card(by = by_i),

        rank_method = method_i

      ) |>

        pull(winnings) |>

        sum()

    })

  )

}
