
find_numbers <- function(lines_in){

  map2(lines_in, 1:length(lines_in), function(line_i, i){

    tbl_split <- tibble(

      row = i,

      number = line_i |>

        # Split each line on anything that's not a digit:
        str_split("[^0-9]") |>

        unlist() |>

        # Toss out anything that was just characters among characters:
        discard(\(x) any(x %in% c("")))

    )

    # Once again, we're going to brute force the string manipulation: we'll
    # gradually consume the line_i string as we identify the start/end positions
    # for the individual numbers we detected. We're doing this - rather than
    # just checking for the first instance of a given number - in case we get
    # repeats, e.g. 153 at chars 24-26 and 100-102...

    # And I've been defeated: this is going to be a `for` loop >:(

    line_so_far <- line_i

    for(row_i in tbl_split){

      endpoints_i <- str_locate(line_so_far, number)

    }
    locations <- reduce(

      .x = tbl_split$numbers,

      .init = tibble(),

      .f = function(endpoint_tbl_so_far, number_i){



      }


    )


    tbl_locations <- bind_cols(

      # Bolt on 2x cols (`start` and `end`) based on finding `number` in

      tbl_split,

      str_locate(line_i, tbl_split$number)

    ) |>

      # mutate(
      #
      #   endpoints = (str_locate(line_i, number))
      #
      # ) |>
      #
      # unpack(endpoints) |>
      identity()

  }) |>

    bind_rows()

}
