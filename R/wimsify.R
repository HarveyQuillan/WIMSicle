#' Convert data to WIMSicle format
#'
#' Converts a data frame or CSV file into the standard
#' WIMSicle observation format.
#'
#' @param data Data frame or CSV filepath.
#' @param sampling_point Column containing sampling point IDs.
#' @param sample_date Column containing sample dates.
#' @param determinand Column containing determinand names.
#' @param result Column containing result values.
#' @param site_name Optional column containing site names.
#' @param unit Optional column containing units.
#' @param determinand_map Optional lookup table with columns:
#'   source_name and determinand_code.
#'
#' @return A tibble in WIMSicle format.
#'
#' @export
WIMSify <- function(
    data,
    sampling_point = NULL,
    sample_date,
    determinand,
    result,
    site_name = NULL,
    unit = NULL,
    determinand_map = NULL
) {

  required <- c(
    sample_date,
    determinand,
    result
  )

  missing <- setdiff(
    required,
    names(data)
  )

  if (length(missing) > 0) {

    stop(
      "Missing columns: ",
      paste(missing, collapse = ", ")
    )

  }

  # Read csv if filepath supplied
  if (
    is.character(data) &&
    length(data) == 1 &&
    file.exists(data)
  ) {

    data <- utils::read.csv(
      data,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

  }

  # Create WIMSicle format
  out <- tibble::tibble(

    sampling_point =
      if (!is.null(sampling_point))
        data[[sampling_point]]
    else
      NA_character_,

    site_name =
      if (!is.null(site_name))
        data[[site_name]]
    else
      NA_character_,

    sample_date =
      as.POSIXct(
        data[[sample_date]],
        tz = "UTC"
      ),

    determinand_code =
      NA_character_,

    determinand =
      data[[determinand]],

    original_determinand =
      data[[determinand]],

    result_raw =
      as.character(
        data[[result]]
      ),

    result =
      suppressWarnings(
        as.numeric(
          gsub(
            "[<>]",
            "",
            data[[result]]
          )
        )
      ),

    unit =
      if (!is.null(unit))
        data[[unit]]
    else
      NA_character_,

    sample_material =
      NA_character_,

    sampling_purpose =
      NA_character_

  )

  # Apply determinand mapping
  if (!is.null(determinand_map)) {

    out <- out |>
      dplyr::left_join(
        determinand_map,
        by = c(
          "determinand" = "source_name"
        )
      )

    out$determinand_code <- dplyr::coalesce(
      out$determinand_code.y,
      out$determinand_code.x
    )

    out <- out |>
      dplyr::select(
        -determinand_code.x,
        -determinand_code.y
      )

  }

  # Lookup official WIMS names
  if (
    "determinand_code" %in% names(out) &&
    any(!is.na(out$determinand_code))
  ) {

    det_lookup <- get_determinands()

    out <- out |>
      dplyr::left_join(
        det_lookup,
        by = "determinand_code",
        suffix = c("", "_wims")
      )

    out$determinand <- dplyr::coalesce(
      out$determinand_wims,
      out$determinand
    )

    out <- out |>
      dplyr::select(
        -determinand_wims
      )

  }

  out

}
