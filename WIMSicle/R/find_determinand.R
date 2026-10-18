#' Find determinands by name
#'
#' Searches a determinand lookup table for matching determinand names.
#'
#' @param search Character string to search for.
#' @param determinand_df Optional determinand lookup table.
#'   If NULL, all determinands will be downloaded using
#'   `get_determinands()`.
#'
#' @return A tibble of matching determinands.
#'
#' @examples
#' find_determinand("nitrate")
#'
#' @export
find_determinand <- function(
    search,
    determinand_df = NULL
) {

  if (is.null(determinand_df)) {
    determinand_df <- get_determinands()
  }

  determinand_df |>
    dplyr::filter(
      grepl(
        search,
        determinand,
        ignore.case = TRUE
      )
    ) |>
    dplyr::arrange(determinand)

}
