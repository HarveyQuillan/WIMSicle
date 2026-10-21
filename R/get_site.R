#' Download water quality observations for a sampling point
#'
#' Retrieves water quality observations from the Environment Agency
#' Water Quality API for a single sampling point.
#'
#' Observations can be filtered by date range and, optionally,
#' by determinand name or determinand code. When determinand names are
#' supplied, the function automatically matches them to available
#' determinands at the sampling point before downloading results.
#'
#' All available pages of results are retrieved automatically and
#' combined into a single tibble.
#'
#' @param sample_point_id Character. Environment Agency sampling point
#'   identifier.
#' @param from Character or Date. Start date for the search period.
#'   Partial dates such as `"2020"` or `"2020-01"` are accepted.
#' @param to Character or Date. End date for the search period.
#'   Partial dates such as `"2020"` or `"2020-12"` are accepted.
#' @param determinands Optional character vector of determinand names
#'   and/or determinand codes. Partial determinand names are supported
#'   and will be matched against determinands available at the site.
#'
#' @return
#' A tibble containing water quality observations with the following
#' columns:
#' \describe{
#'   \item{sampling_point}{Sampling point identifier.}
#'   \item{site_name}{Sampling point name.}
#'   \item{sample_date}{Sample date and time (UTC).}
#'   \item{determinand_code}{Environment Agency determinand code.}
#'   \item{determinand}{Environment Agency determinand name.}
#'   \item{result}{Numeric result value where available.}
#'   \item{result_raw}{Original result value as returned by the API.}
#'   \item{unit}{Measurement unit.}
#'   \item{sample_material}{Sample material type.}
#'   \item{sampling_purpose}{Sampling purpose description.}
#' }
#'
#' Results are returned in chronological order. If no observations are
#' found, an empty observations tibble is returned.
#'
#' @examples
#' # Download all observations
#' get_site(
#'   sample_point_id = "TH-PCNR0051"
#' )
#'
#' # Restrict by date range
#' get_site(
#'   sample_point_id = "TH-PCNR0051",
#'   from = "2020-01-01",
#'   to = "2020-12-31"
#' )
#'
#' # Filter by determinand name
#' get_site(
#'   sample_point_id = "TH-PCNR0051",
#'   determinands = "Nitrate"
#' )
#'
#' # Filter by multiple determinands
#' get_site(
#'   sample_point_id = "TH-PCNR0051",
#'   determinands = c(
#'     "Nitrate",
#'     "Phosphate"
#'   )
#' )
#'
#' @seealso
#' \code{\link{get_site_determinands}},
#' \code{\link{get_sites}},
#' \code{\link{find_determinand}}
#'
#' @export
get_site <- function(
    sample_point_id,
    from = "2000-01-01",
    to = Sys.Date(),
    determinands = NULL
) {

  from <- parse_wims_date(from)
  to <- parse_wims_date(to)

  if (!is.character(sample_point_id) ||
      length(sample_point_id) != 1) {
    stop(
      "'sample_point_id' must be a single character value"
    )
  }

  validate_date_range(from, to)

  base_url <- paste0(
    .wims_base_url,
    "sampling-point/",
    sample_point_id,
    "/observation"
  )

  if (!is.null(determinands)) {

    site_dets <- get_site_determinands(
      sample_point_id = sample_point_id,
      from = from,
      to = to
    )

    matched_dets <- resolve_determinands(
      determinands,
      site_dets
    )

    if (length(matched_dets) == 0) {
      stop(
        "No matching determinands found."
      )
    }
  }

  skip <- 0
  limit <- .wims_page_limit
  page <- 1
  max_pages <- 1000

  results <- list()

  repeat {

    if (page > max_pages) {
      stop(
        "Maximum page limit exceeded."
      )
    }

    req <- httr2::request(base_url) |>
      httr2::req_headers(
        Accept = "application/ld+json",
        `API-Version` = "1"
      ) |>
      httr2::req_url_query(
        skip = skip,
        limit = limit,
        dateFrom = from,
        dateTo = to,
        complianceOnly = "false"
      )

    if (!is.null(determinands)) {
      req <- req |>
        httr2::req_url_query(
          determinand = paste(
            matched_dets,
            collapse = ","
          )
        )
    }

    resp <- perform_wims_request(req)

    json <- httr2::resp_body_json(resp)

    records <- json$member

    if (length(records) == 0) {
      break
    }

    page_df <- purrr::map_dfr(
      records,
      parse_observation
    )

    results[[length(results) + 1]] <- page_df

    if (nrow(page_df) < limit) {
      break
    }

    skip <- skip + limit
    page <- page + 1
  }

  if (length(results) == 0) {
    return(empty_observations())
  }

  dplyr::bind_rows(results) |>
    dplyr::mutate(
      sample_date = as.POSIXct(
        sample_date,
        tz = "UTC"
      )
    ) |>
    dplyr::arrange(sample_date)
}
