
tidy_day_06_data <- function(raw_lines){

  extract_digits <- \(x) x |>
    str_squish() |>
    str_split(" ") |>
    unlist() |>
    keep(\(y) str_detect(y, "[:digit:]")) |>
    as.numeric()

  tibble(

    time = extract_digits(raw_lines[1]),

    distance = extract_digits(raw_lines[2])

  ) |>

    mutate(race = 1:n(), .before = 1)

}

expand_potential_strategies <- function(race_info_table){

  # d = v * t
  #
  # d_target <= t_held * (t_total - t_held)
  #
  #          <= t_held * t_total - t_held^2


  race_info_table |>

    group_by(race, time, distance) |>

    reframe(
      t_held = 0:time,
      t_moving = time:0
    ) |>

    ungroup() |>

    mutate(d_travelled = t_held * t_moving) |>

    mutate(beat_record = d_travelled > distance)


}


reinterpret_day_06_data <- function(race_info_table){

  race_info_table |>

    summarise(
      race = 1,
      time = paste(time, collapse = "") |> as.numeric(),
      distance = paste(distance, collapse = "") |> as.numeric()
    )

}


pinpoint_winning_strategies <- function(reinterpreted_race_table){

  # So d_travelled = t_held * t_total - t_held^2

  # I.e. negative quadratic

  # So if we want where d_travelled < d_record

  # We're looking for values above the two intersections of this function and
  # our d_record value (straight line)

  # d_record = t_held * t_total - t_held^2
  #
  # =>     0 = -t_held^2 + t_total*t_held - d_record

  # So given that ax^2 + bx + c = 0
  # when x = [-b +/- sqrt(b^2 - 4ac)] / 2a

  quaddie <- function(a, b, c){

    determinant <- sqrt(b^2 - 4*a*c)

    return(c(
      positive = (-b + determinant) / 2*a,
      negative = (-b - determinant) / 2*a
    ))

  }


  reinterpreted_race_table |>

    mutate(

      solutions = list(quaddie(a = -1, b = time, c = -1*distance))

    ) |>

    unnest_wider(solutions) |>

    mutate(

      distance_positive = positive * (time - positive),

      distance_negative = negative * (time - negative),

      verify_positive = distance_positive == distance,

      verify_negative = distance_negative == distance,

      # So take the number of integers between these two solutions - i.e., the
      # curve of the negative quadratic that is *above* our d_record line -
      # being sure to -1 as it's outer bounds exclusive (the intersections with
      # the d_record line do not *beat* the record, just equal it):

      n_winning_solutions = max(c(negative, positive)) - min(c(negative, positive)) - 1

    ) |>

    identity()

}
