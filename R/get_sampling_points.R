#' Download sampling point metadata
#'
#' Downloads sampling point information from the
#' Environment Agency Water Quality API.
#'
#' @param point_notation Optional sampling point ID(s).
#' @param site_name Optional partial site name search.
#' @param region_code Optional region code.
#' @param area_code Optional area code.
#' @param sub_area_code Optional sub-area code.
#' @param status_code Optional status code.
#' @param type_code Optional sampling point type code.
#'
#' @return A tibble containing sampling point metadata.
#'
#' @examples
#' get_sampling_points()
#'
#' get_sampling_points(
#'   region_code = "TH"
#' )
#'
#' open_river_sites <- get_sampling_points(
#'   status_code = "O",
#'   type_code = "F6"
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
