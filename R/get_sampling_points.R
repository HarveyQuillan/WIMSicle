#' Download sampling point metadata
#'
#' Retrieves sampling point metadata from the Environment Agency Water
#' Quality API.
#'
#' Results can be filtered using sampling point identifiers, site names,
#' organisational geography (region, area and sub-area), sampling point
#' status, sampling point type, or by proximity to a National Grid
#' location.
#'
#' The function automatically retrieves all available pages of results
#' and returns them as a single tibble.
#'
#' @param point_notation Optional character vector of sampling point
#'   identifiers.
#' @param site_name Optional partial site name search string.
#' @param region_code Optional Environment Agency region code.
#' @param area_code Optional Environment Agency area code.
#' @param sub_area_code Optional Environment Agency sub-area code.
#' @param status_code Optional sampling point status code.
#' @param type_code Optional sampling point type code.
#' @param easting Optional British National Grid easting.
#' @param northing Optional British National Grid northing.
#' @param radius Optional search radius in kilometres. Must be supplied
#'   with both `easting` and `northing`.
#'
#' @return
#' A tibble containing sampling point metadata, including:
#' \describe{
#'   \item{sampling_point}{Sampling point identifier.}
#'   \item{site_name}{Sampling point name.}
#'   \item{status}{Sampling point status description.}
#'   \item{status_code}{Sampling point status code.}
#'   \item{type}{Sampling point type description.}
#'   \item{type_code}{Sampling point type code.}
#'   \item{region}{Environment Agency region.}
#'   \item{region_code}{Environment Agency region code.}
#'   \item{area}{Environment Agency area.}
#'   \item{area_code}{Environment Agency area code.}
#'   \item{sub_area}{Environment Agency sub-area.}
#'   \item{sub_area_code}{Environment Agency sub-area code.}
#'   \item{easting}{British National Grid easting.}
#'   \item{northing}{British National Grid northing.}
#' }
#'
#' If no matching sampling points are found, an empty tibble is returned.
#'
#' @examples
#' # Find a specific sampling point
#' get_sampling_points(
#'   point_notation = "TH-PCNR0051"
#' )
#'
#' # Search by region
#' get_sampling_points(
#'   region_code = "TH"
#' )
#'
#' # Open river monitoring sites
#' get_sampling_points(
#'   status_code = "O",
#'   type_code = "F6"
#' )
#'
#' # Sites within 10 km of a grid reference
#' get_sampling_points(
#'   easting = 530000,
#'   northing = 180000,
#'   radius = 10
#' )
#'
#' @export
get_sampling_points <- function(
    point_notation = NULL,
    site_name = NULL,
    region_code = NULL,
    area_code = NULL,
    sub_area_code = NULL,
    status_code = NULL,
    type_code = NULL,
    easting = NULL,
    northing = NULL,
    radius = NULL
) {

  url <- paste0(
    .wims_base_url,
    "sampling-point"
  )

  skip <- 0
  limit <- .wims_page_limit

  results <- list()

  repeat {

    req <- httr2::request(url) |>
      httr2::req_headers(
        Accept = "application/ld+json",
        `API-Version` = "1"
      ) |>
      httr2::req_url_query(
        skip = skip,
        limit = limit
      )

    if (!is.null(point_notation)) {

      req <- req |>
        httr2::req_url_query(
          pointNotation = point_notation
        )

    }

    if (!is.null(site_name)) {

      req <- req |>
        httr2::req_url_query(
          prefLabel = site_name
        )

    }

    if (!is.null(region_code)) {

      req <- req |>
        httr2::req_url_query(
          region = region_code
        )

    }

    if (!is.null(area_code)) {

      req <- req |>
        httr2::req_url_query(
          area = area_code
        )

    }

    if (!is.null(sub_area_code)) {

      req <- req |>
        httr2::req_url_query(
          subArea = sub_area_code
        )

    }

    if (!is.null(status_code)) {

      req <- req |>
        httr2::req_url_query(
          samplingPointStatus = status_code
        )

    }

    if (!is.null(type_code)) {

      req <- req |>
        httr2::req_url_query(
          samplingPointType = type_code
        )

    }

    if (!is.null(easting)) {

      req <- req |>
        httr2::req_url_query(
          easting = easting
        )

    }

    if (!is.null(northing)) {

      req <- req |>
        httr2::req_url_query(
          northing = northing
        )

    }

    if (!is.null(radius)) {

      req <- req |>
        httr2::req_url_query(
          radius = radius
        )

    }

    resp <- perform_wims_request(req)

    json <- httr2::resp_body_json(resp)

    total_items <- json$totalItems

    message(
      sprintf(
        "Downloaded %s of %s sampling points",
        format(min(skip + limit, total_items), big.mark = ","),
        format(total_items, big.mark = ",")
      )
    )

    records <- json$member

    if (length(records) == 0) {
      break
    }

    page_df <- purrr::map_dfr(
      records,
      parse_sampling_point
    )

    results[[length(results) + 1]] <- page_df

    if (nrow(page_df) < limit) {
      break
    }

    skip <- skip + limit

  }

  if (length(results) == 0) {

    message(
      "No sampling points found matching supplied filters."
    )

    return(
      empty_sampling_points()
    )

  }

  dplyr::bind_rows(results)

}
