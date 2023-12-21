
extract_calibration_digits <- function(strings_in,
                                       search_string = "[:digit:]",
                                       search_function = str_extract_all){

  map_chr(strings_in, function(string_i){

    digits <- search_function(string_i, search_string) |> unlist()

    assert_that(
      length(digits) > 0,
      # This wasn't specified in the input text - so presumably won't happen.
      # But let's code defensively :)
      msg = "No digits found - edge case!"
    )

    return(
      paste0(
        first(digits),
        last(digits)
      )
    )

  })

}


digitify_strings <- function(strings_in){

  reduce2(

    # We could move .x and .y out as parameters so we could include/exclude zero
    # more easily - the point here is that we just need a 'dict' structure.

    .x = c("one",
           "two",
           "three",
           "four",
           "five",
           "six",
           "seven",
           "eight",
           "nine"),

    .y = as.character(1:9),

    # For each search-term / replacement-term pair,
    function(modified_calibration_strings,
             search_term_j,
             replacement_j){

      # Replace all instance in each string in the list.
      str_replace_all(
        modified_calibration_strings,
        search_term_j,
        replacement_j
      )

    },

    .init = strings_in

  )

}

extract_without_consuming <- function(string_in,
                                      search_string){

  reduce(

    .x = 1:nchar(string_in),

    function(matches_so_far, i){

      append(
        matches_so_far,
        str_extract(
          string = str_sub(string_in, start = i, end = -1L),
          pattern = paste0("^", search_string)
        )
      )

    },

    .init = list()

  ) |> discard(is.na) # Tidy up any NAs...

}
