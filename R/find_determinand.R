#' Find determinands by name
#'
#' Searches a determinand lookup table and returns determinands whose
#' names contain the supplied search text. Matching is case-insensitive
#' and uses fixed string matching rather than regular expressions.
#'
#' This function is useful for discovering Environment Agency WIMS
#' determinand names and codes before using functions such as
#' `get_site()` or `get_sites()`.
#'
#' @param search Character string used to search determinand names.
#' @param determinand_df A determinand lookup table containing at least
#'   a `determinand` column. Defaults to `determinand_lookup`.
#'
#' @return
#' A tibble containing matching determinands, ordered alphabetically by
#' determinand name.
#'
#' @examples
#' find_determinand("nitrate")
#'
#' find_determinand("phosph")
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
