
read_day_08_instrx <- function(raw_lines){

  raw_lines[1] |>

    str_split("") |>

    unlist()

}

read_day_08_maps <- function(raw_lines){

  raw_lines |>

    tail(-1) |>

    discard(\(x){x == ""}) |>

    map(function(line_i){

      list(

        from_map = str_extract(line_i, "^.+(?= \\=)"),

        L = str_extract(line_i, "(?<=\\().+(?=,)"),

        R = str_extract(line_i, "(?<=, ).+(?=\\))")

      )

    }) |>

    bind_rows()

}

navigate <- function(starting_map,
                     instrx,
                     maps){

  current_map <- starting_map

  instrx_counter <- 1

  mod_denominator <- length(instrx)

  steps <- 0

  while(current_map != "ZZZ"){

    steps <- steps + 1

    mod_counter_raw <- steps %% mod_denominator

    # If it's e.g. 269 %% 269 = 0, add 269 to get back to the right index (this is
    # not a proper modulo, I guess)
    mod_counter <- mod_counter_raw + (mod_counter_raw == 0)*mod_denominator


    this_instrx <- instrx[mod_counter]

    current_map <- maps[[which(maps$from_map == current_map), this_instrx]]

    if(steps %% 1000 == 0){
      cat(glue("Steps: {steps}"))
      cat("\n")
    }

  }

  message(glue("Result: {steps} steps"))

  return(invisible(steps))

}


navigate_simultaneously <- function(starting_maps = c("AAA"),
                                    done_criteria = "^ZZZ$",
                                    instrx,
                                    maps,
                                    debug = FALSE,
                                    debug_step_interval = 1000){

  # We're going to convert maps back to a basic data frame so we can do quick
  # lookups based on row names.
  maps <- as.data.frame(maps)
  rownames(maps) <- maps$from_map


  current_maps <- starting_maps

  instrx_counter <- 1

  mod_denominator <- length(instrx)

  steps <- 0

  done <- FALSE

  while(!done){

    steps <- steps + 1

    # Because we're 1-indexed and our array goes from 1 to n, we need to deduct
    # 1 before modulo then add 1 back on after:

    mod_counter <- ((steps - 1) %% mod_denominator) + 1


    this_instrx <- instrx[mod_counter]


    current_maps <- maps[current_maps, this_instrx]

    # Which means we need a different "done" condition:

    done <- all(grepl(done_criteria, current_maps))

    if(debug & steps %% debug_step_interval == 0){
      cat(glue("Steps: {steps}"))
      cat("\n")
    }

  }

  message(glue("Result: {steps} steps"))

  return(invisible(steps))

}
