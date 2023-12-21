

read_raw_day_05_data <- function(path_to_raw_data){

  # We want to ensure that our raw data ends with a blank line for consistency:

  raw_data <- read_lines(path_to_raw_data)

  if(tail(raw_data, 1) != ""){

    raw_data <- c(raw_data, "")

  }

  return(raw_data)

}

fetch_seed_table <- function(raw_lines){

  tibble(

    seed = raw_lines |>

      keep(\(x) str_starts(x, "seeds:")) |>

      str_squish() |>

      str_split(" ") |>

      unlist() |>

      keep(\(x) str_starts(x, "[:digit:]")) |>

      as.numeric()

  )

}


fetch_seed_table_v2 <- function(raw_lines){

  # Not Yet Implemented

  message("Returning v1 seed table...")

  fetch_seed_table(raw_lines)


}


fetch_input_map <- function(raw_lines,
                            source_label,
                            destination_label,
                            debug = FALSE){

  # Find the bounds of the section in question ---------------------------

  header_to_find <- glue("{source_label}-to-{destination_label}")

  line_initial <- grep(header_to_find, raw_lines)

  assert_that(
    length(line_initial) == 1,
    msg = glue("{header_to_find} not found or matched multiple!")
  )

  blank_lines <- grep("^$", raw_lines)

  # Find the first blank line *after* the header of a section
  line_final <- min(blank_lines[which(blank_lines > line_initial)])

  if(debug){
    print(glue("Initial @ {line_initial}: {raw_lines[line_initial]}"))
    print(glue("Final   @ {line_final}: {raw_lines[line_final]}"))
  }

  # Extract the section --------------------------------------------------

  source_start <- glue("{source_label}_start" )
  source_end   <- glue("{source_label}_end")
  destination_start <- glue("{destination_label}_start")
  destination_end   <- glue("{destination_label}_end")
  range_col <- glue("range_{source_label}_to_{destination_label}")


  raw_lines[(line_initial + 1):(line_final - 1)] |>

    map(function(line_i){

      components <- line_i |>
        str_squish() |>
        str_split(" ") |>
        unlist() |>
        as.numeric()

      # Note also that the range logic means that you need to add the range - 1,
      # as the bounds are inclusive

      return(
        tibble(

          {{ source_start }} := components[2],
          {{ source_end   }} := components[2] + (components[3] - 1),

          {{ destination_start }} := components[1],
          {{ destination_end   }} := components[1] + (components[3] - 1),

          {{ range_col }} := components[3]

        )
      )

    }) |>

    bind_rows() |>

    return()

}




join_and_pinpoint <- function(table_x,
                              mapping_table_y,
                              from_dimension,
                              to_dimension){

  sym_from       <- sym(glue("{from_dimension}"))
  sym_from_start <- sym(glue("{from_dimension}_start"))
  sym_from_end   <- sym(glue("{from_dimension}_end"))

  sym_to       <- sym(glue("{to_dimension}"))
  sym_to_start <- sym(glue("{to_dimension}_start"))

  left_join(

    table_x,

    mapping_table_y,

    by = join_by(
      between(
        {{ sym_from }},
        {{ sym_from_start }},
        {{ sym_from_end   }}
      ))

  ) |>

    mutate(
      {{ sym_to }} := {{ sym_from }} - {{ sym_from_start }} + {{ sym_to_start }}
    ) |>

    # And now the extra kicker from the guide: anything not mapped to something
    # else is mapped to itself. Easy to miss lol.
    mutate(
      {{ sym_to }} := coalesce( {{ sym_to }}, {{ sym_from }} )
    ) |>

    # And we have to drop the extra info because otherwise we'd end up with like
    # soil_starts.x.x, soil_starts.x.y, soil_starts.y.x, ...
    select(
      -starts_with("range_"),
      -ends_with("_start"),
      -ends_with("_end")
    )

}


join_everything <- function(tbl_seeds,
                            all_maps){

  reduce(

    .x = all_maps,

    .init = tbl_seeds,

    .f = function(joined_so_far, map_i){

      join_and_pinpoint(
        joined_so_far,
        map_i$map,
        from_dimension = map_i$from,
        to_dimension = map_i$to
      )

    }

  )

}
