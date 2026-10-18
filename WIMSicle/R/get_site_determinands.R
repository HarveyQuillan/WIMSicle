#' Get determinands available at a site
#'
#' @param sample_point_id Character. Sampling point ID.
#' @param from Character or Date. Start date.
#' @param to Character or Date. End date.
#'
#' @return A tibble containing determinand codes and names.
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
