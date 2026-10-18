#' Download WIMS determinand lookup table
#'
#' Downloads all available determinands from the Environment Agency
#' Water Quality API.
#'
#' @return A tibble containing determinand codes and names.
#'
#' @examples
#' dets <- get_determinands()
#'
#' @export
get_determinands <- function() {

  url <- paste0(
    .wims_base_url,
    "codelist/determinand"
  )

  skip <- 0
  limit <- .wims_page_limit

  results <- list()

  repeat {

    resp <- httr2::request(url) |>
      httr2::req_headers(
        Accept = "application/ld+json",
        `API-Version` = "1"
      ) |>
      httr2::req_url_query(
        skip = skip,
        limit = limit
      ) |>
      httr2::req_perform()

    json <- httr2::resp_body_json(resp)

    records <- json$member

    if (length(records) == 0) {
      break
    }

    page_df <- purrr::map_dfr(
      records,
      parse_determinand
    )

    results[[length(results) + 1]] <- page_df

    if (nrow(page_df) < limit) {
      break
    }

    skip <- skip + limit

  }

  dplyr::bind_rows(results) |>
    dplyr::distinct() |>
    dplyr::arrange(determinand)

}
