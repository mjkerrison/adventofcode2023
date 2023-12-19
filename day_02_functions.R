
extract_game_num <- \(x) str_extract(x, "(?<=^Game )[:digit:]+(?=:)")

extract_cube_count <- \(x, colour) str_extract(x, glue("[:digit:]+(?= {colour})"))


clean_day_02_data <- function(lines_in){

  lines_in %>%

    map(function(line_i){

      # Start by getting the information from each round:
      line_i %>% str_split(";") %>%

        map(function(round_i){

          list(
            blue = extract_cube_count(round_i, "blue"),
            red = extract_cube_count(round_i, "red"),
            green = extract_cube_count(round_i, "green")
          )

        }) %>%

        bind_rows() %>%

        # Label those rounds accordingly:
        mutate(round = 1:n(), .before = 1) %>%

        # Then add the game round ID:
        mutate(game_id = extract_game_num(line_i), .before = 1)

    }) %>%

    bind_rows() %>%

    # Tidy some typing, because we were extracting characters:
    mutate(
      across(
        everything(),
        as.numeric
      )
    ) %>%

    # And tidy up NAs:
    replace_na(
      list(
        blue = 0,
        red = 0,
        green = 0
      )
    )

}

restrict_games_by_colour <- function(games_tbl,
                                     ...){

  constraint_list <- enquos(...)

  games_tbl %>%

    group_by(game_id) %>%

    filter(
      !!!(constraint_list)
    ) %>%

    ungroup()

}

crunch_game_requirements <- function(games_tbl){

  games_tbl %>%

    group_by(game_id) %>%

    summarise(
      across(
        c(red, green, blue),
        max
      )
    ) %>%

    ungroup() %>%

    mutate(game_power = red * green * blue)

}
