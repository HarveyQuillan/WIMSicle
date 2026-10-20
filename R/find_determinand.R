#' Find determinands by name
#'
#' Searches a determinand lookup table for matching determinand names.
#'
#' @param search Character string to search for.
#' @param determinand_df Optional determinand lookup table.
#'
#' @return A tibble of matching determinands.
#'
#' @examples
#' find_determinand("nitrate")
#'
#' @export
find_determinand <- function(
    search,
    determinand_df = determinand_lookup
) {
  determinand_df |>
    dplyr::filter(
      grepl(
        search,
        determinand,
        ignore.case = TRUE,
        fixed = TRUE
      )
    ) |>
    dplyr::arrange(determinand)
}
