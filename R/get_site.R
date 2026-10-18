#' Download water quality observations for a sampling point
#'
#' Downloads water quality observations from the Environment Agency
#' Water Quality API for a single sampling point.
#'
#' @param sample_point_id Character. Sampling point ID.
#' @param from Character or Date. Start date.
#' @param to Character or Date. End date.
#' @param determinands Optional character vector of determinand names
#'   or codes.
#'
#' @return A tibble of water quality observations.
#'
#' @examples
#' get_site(
#'   sample_point_id = "TH-PCNR0051",
#'   from = "2020-01-01",
#'   to = "2020-12-31"
#' )
#'
#' @export
# TODO:
# Cache site determinands to avoid repeated downloads.
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
    stop("'sample_point_id' must be a single character value")
  }

  if (as.Date(from) > as.Date(to)) {
    stop("'from' must be before 'to'")
  }

  base_url <- paste0(
    .wims_base_url,
    "sampling-point/",
    sample_point_id,
    "/observation"
  )

  skip <- 0
  limit <- .wims_page_limit

  results <- list()

  repeat {

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

      site_dets <- get_site_determinands(
        sample_point_id = sample_point_id,
        from = from,
        to = to
      )

      matched_dets <- resolve_determinands(
        determinands,
        site_dets
      )

      req <- req |>
        httr2::req_url_query(
          determinand = paste(
            matched_dets,
            collapse = ","
          )
        )
    }

    resp <- req |>
      httr2::req_perform()

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

  }

  if (length(results) == 0) {

    message(
      "No data found for site '",
      sample_point_id,
      "' between ",
      from,
      " and ",
      to
    )

    return(
      tibble::tibble(
        sampling_point = character(),
        site_name = character(),
        sample_date = as.POSIXct(character(), tz = "UTC"),
        determinand_code = character(),
        determinand = character(),
        result = numeric(),
        unit = character(),
        sample_material = character(),
        sampling_purpose = character()
      )
    )

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
