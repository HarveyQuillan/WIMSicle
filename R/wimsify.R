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
#' A tibble in WIMSicle observation format containing:
#' \describe{
#'   \item{sampling_point}{Sampling point identifier.}
#'   \item{site_name}{Sampling point name.}
#'   \item{sample_date}{Sample date and time (UTC).}
#'   \item{determinand_code}{Environment Agency determinand code.}
#'   \item{determinand}{Standardised determinand name.}
#'   \item{original_determinand}{Original source determinand name.}
#'   \item{result_raw}{Original result value.}
#'   \item{result}{Numeric result value where possible.}
#'   \item{unit}{Measurement unit.}
#'   \item{sample_material}{Sample material type.}
#'   \item{sampling_purpose}{Sampling purpose description.}
#' }
#'
#' @details
#' Result values are converted to numeric where possible. Common
#' qualifier symbols such as `<` and `>` are removed before numeric
#' conversion.
#'
#' Missing optional fields are populated with `NA`.
#'
#' If a determinand mapping table is supplied, determinand codes are
#' joined into the output and official Environment Agency determinand
#' names are automatically applied where available.
#'
#' @examples
#' # Convert a data frame
#' wimsify(
#'   data = my_data,
#'   sampling_point = "site_id",
#'   sample_date = "sample_date",
#'   determinand = "parameter",
#'   result = "value"
#' )
#'
#' # Convert a CSV file
#' wimsify(
#'   data = "results.csv",
#'   sample_date = "Date",
#'   determinand = "Parameter",
#'   result = "Result"
#' )
#'
#' # Apply determinand mapping
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

    det_lookup <- determinand_lookup

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
