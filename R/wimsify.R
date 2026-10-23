#' Convert data to WIMSicle format
#'
#' Converts a data frame or CSV file into the standard WIMSicle
#' observation format.
#'
#' The function standardises column names, converts dates and results
#' into WIMS-compatible fields, and optionally maps source determinand
#' names to Environment Agency determinand codes.
#'
#' When determinand codes are available, official Environment Agency
#' determinand names are automatically applied using the package
#' determinand lookup table.
#'
#' @param data A data frame containing water quality observations, or a
#'   character string containing the path to a CSV file.
#' @param sampling_point Optional column containing sampling point
#'   identifiers.
#' @param sample_date Column containing sample dates.
#' @param determinand Column containing determinand names.
#' @param result Column containing result values.
#' @param site_name Optional column containing site names.
#' @param unit Optional column containing measurement units.
#' @param determinand_map Optional lookup table containing:
#'   \describe{
#'     \item{source_name}{Original determinand name.}
#'     \item{determinand_code}{Environment Agency determinand code.}
#'   }
#'
#' @return
#' A tibble in WIMSicle observation format.
#'
#' @examples
#' my_data <- data.frame(
#'   site_id = c("SITE1", "SITE1"),
#'   sample_date = c("2024-01-01", "2024-02-01"),
#'   parameter = c("Nitrate", "Phosphate"),
#'   value = c(2.3, 0.15)
#' )
#'
#' wimsify(
#'   data = my_data,
#'   sampling_point = "site_id",
#'   sample_date = "sample_date",
#'   determinand = "parameter",
#'   result = "value"
#' )
#'
#' my_mapping <- data.frame(
#'   source_name = c("Nitrate", "Phosphate"),
#'   determinand_code = c("0117", "0061")
#' )
#'
#' wimsify(
#'   data = my_data,
#'   sample_date = "sample_date",
#'   determinand = "parameter",
#'   result = "value",
#'   determinand_map = my_mapping
#' )
#'
#' @seealso
#' \code{\link{find_determinand}}
#'
#' @export
wimsify <- function(
    data,
    sampling_point = NULL,
    sample_date,
    determinand,
    result,
    site_name = NULL,
    unit = NULL,
    determinand_map = NULL
) {

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

  sampling_point_col <- if (is.null(sampling_point)) {
    rep(NA_character_, nrow(data))
  } else {
    data[[sampling_point]]
  }

  site_name_col <- if (is.null(site_name)) {
    rep(NA_character_, nrow(data))
  } else {
    data[[site_name]]
  }

  unit_col <- if (is.null(unit)) {
    rep(NA_character_, nrow(data))
  } else {
    data[[unit]]
  }

  sample_date_col <- as.POSIXct(
    data[[sample_date]],
    tz = "UTC"
  )

  determinand_col <- data[[determinand]]

  result_col <- data[[result]]

  out <- tibble::tibble(
    sampling_point = sampling_point_col,
    site_name = site_name_col,
    sample_date = sample_date_col,
    determinand_code = rep(
      NA_character_,
      nrow(data)
    ),
    determinand = determinand_col,
    original_determinand = determinand_col,
    result_raw = as.character(result_col),
    result = suppressWarnings(
      as.numeric(
        gsub(
          "[<>]",
          "",
          result_col
        )
      )
    ),
    unit = unit_col,
    sample_material = rep(
      NA_character_,
      nrow(data)
    ),
    sampling_purpose = rep(
      NA_character_,
      nrow(data)
    )
  )

  if (!is.null(determinand_map)) {

    out <- dplyr::left_join(
      out,
      determinand_map,
      by = c(
        "determinand" = "source_name"
      )
    )

    out$determinand_code <- dplyr::coalesce(
      out$determinand_code.y,
      out$determinand_code.x
    )

    out <- dplyr::select(
      out,
      -determinand_code.x,
      -determinand_code.y
    )

  }

  if (
    "determinand_code" %in% names(out) &&
    any(!is.na(out$determinand_code))
  ) {

    out <- dplyr::left_join(
      out,
      determinand_lookup,
      by = "determinand_code",
      suffix = c("", "_wims")
    )

    out$determinand <- dplyr::coalesce(
      out$determinand_wims,
      out$determinand
    )

    out <- dplyr::select(
      out,
      -determinand_wims
    )

  }

  out

}
