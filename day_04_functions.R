
process_day_04_data <- function(raw_data){

  map(raw_data, function(line_i){

    return(
      rev(
        within(
          list(),
          {

            target_numbers <- line_i |>
              str_extract("(?<=: ).+(?= \\|)") |>
              str_squish() |>
              str_split(" ") |>
              unlist() |>
              as.numeric()

            card_numbers <- line_i |>
              str_extract("(?<=\\| ).+") |>
              str_squish() |>
              str_split(" ") |>
              unlist() |>
              as.numeric()

            winning_numbers <- card_numbers[which(card_numbers %in% target_numbers)]

            n_wins <- length(winning_numbers)

            value <- (n_wins > 0) * 2^(n_wins - 1)

          }
        )
      )
    )

  })

}


extract_card_attribute_tbl <- function(processed_data,
                                       target_attribute){

  tibble(

    id = 1:length(processed_data),

    !!sym(target_attribute) := processed_data |>
      map(\(x) pluck(x, target_attribute)) |>
      unlist()

  )

}


find_card_copies <- function(id_wins_table){

  winning_dictionary <- bind_rows(

    # Cards that don't win anything - we'll keep them in here for easier
    # arithmetic / joining / checking later (as they don't spawn new cards, but
    # we can end up with multiple copies *of them*)
    id_wins_table |>
      filter(n_wins == 0) |>
      select(-n_wins) |>
      mutate(cards_won = 0),

    # Cards that do win more cards
    id_wins_table |>

      filter(n_wins > 0) |>

      group_by(id) |>

      reframe(
        cards_won = (id + 1):(id + n_wins)
      ) |>

      ungroup()

  ) |>

    arrange(id, cards_won)


  # cumulative_n_won <- reduce(
  #
  #   # For each card,
  #   .x = unique(id_wins_table$id),
  #
  #   # Tally the number of times it's been won by previous cards,
  #   # Multiplied by the number of those previous cards,
  #   #
  #   .f = function(tbl_so_far, id_i){
  #
  #
  #
  #   },
  #
  #
  #   # Start with a table of 1 of each card - that's what we started with
  #   .init = tibble(id = unique(id_wins_table$id),
  #                  cumulative_n = 1)
  #
  #
  # )

}
