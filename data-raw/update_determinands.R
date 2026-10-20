# data-raw/update_determinands.R

library(httr2)
library(dplyr)
library(purrr)
library(tibble)
library(usethis)

download_determinands <- function() {

  base_url <- "https://environment.data.gov.uk/water-quality/"
  url <- paste0(
    base_url,
    "codelist/determinand"
  )

  skip <- 0
  limit <- 250L
  max_pages <- 1000L
  page <- 1L

  results <- list()

  repeat {

    if (page > max_pages) {
      stop(
        "Maximum page limit exceeded."
      )
    }

    req <- httr2::request(url) |>
      httr2::req_headers(
        Accept = "application/ld+json",
        `API-Version` = "1"
      ) |>
      httr2::req_url_query(
        skip = skip,
        limit = limit
      ) |>
      httr2::req_retry(
        max_tries = 5
      ) |>
      httr2::req_timeout(
        30
      )

    resp <- httr2::req_perform(req)

    json <- httr2::resp_body_json(resp)

    records <- json$member

    if (length(records) == 0) {
      break
    }

    page_df <- purrr::map_dfr(
      records,
      \(x) tibble::tibble(
        determinand_code = x$notation %||% NA_character_,
        determinand = x$prefLabel %||% NA_character_
      )
    )

    results[[length(results) + 1]] <- page_df

    if (nrow(page_df) < limit) {
      break
    }

    skip <- skip + limit
    page <- page + 1L
  }

  if (length(results) == 0) {

    return(
      tibble::tibble(
        determinand_code = character(),
        determinand = character()
      )
    )

  }

  dplyr::bind_rows(results) |>
    dplyr::distinct() |>
    dplyr::arrange(determinand)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

determinand_lookup <- download_determinands()

usethis::use_data(
  determinand_lookup,
  overwrite = TRUE,
  compress = "xz"
)
