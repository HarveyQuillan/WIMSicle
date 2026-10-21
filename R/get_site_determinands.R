#' Get determinands available at a sampling point
#'
#' Retrieves all water quality observations for a sampling point within
#' the specified date range and returns the unique determinands present
#' in the dataset.
#'
#' This function is useful for discovering which determinands are
#' available before downloading observations with `get_site()` or
#' `get_sites()`.
#'
#' @param sample_point_id Character. Environment Agency sampling point
#'   identifier.
#' @param from Character or Date. Start date for the search period.
#'   Partial dates such as `"2020"` or `"2020-01"` are accepted.
#' @param to Character or Date. End date for the search period.
#'   Partial dates such as `"2020"` or `"2020-12"` are accepted.
#'
#' @return
#' A tibble containing one row per determinand with the following
#' columns:
#' \describe{
#'   \item{determinand_code}{Environment Agency determinand code.}
#'   \item{determinand}{Environment Agency determinand name.}
#' }
#'
#' Determinands are returned in alphabetical order.
#'
#' @examples
#' get_site_determinands(
#'   sample_point_id = "TH-PCNR0051"
#' )
#'
#' get_site_determinands(
#'   sample_point_id = "TH-PCNR0051",
#'   from = "2020-01-01",
#'   to = "2020-12-31"
#' )
#'
#' @seealso
#' \code{\link{get_site}},
#' \code{\link{find_determinand}}
#'
#' @export
get_site_determinands <- function(
    sample_point_id,
    from = "2000-01-01",
    to = Sys.Date()

) {

  from <- parse_wims_date(from)
  to <- parse_wims_date(to)

  data <- get_site(
    sample_point_id = sample_point_id,
    from = from,
    to = to
  )

  extract_determinands(data)

}
