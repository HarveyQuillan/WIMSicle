#' Download observations for multiple sites
#'
#' Downloads water quality observations for multiple sampling points
#' and combines the results into a single tibble.
#'
#' @param sites Character vector of sample point IDs.
#' @param from Start date.
#' @param to End date.
#' @param determinands Optional vector of determinands.
#'
#' @return Tibble of observations from all sites.
#'
#' @examples
#' get_sites(
#'   sites = c(
#'     "TH-PCNR0051",
#'     "TH-PNAR0250"
#'   ),
#'   from = "2020-01-01",
#'   to = "2021-01-01"
#' )
#'
#' @export
get_sites <- function(
    sites,
    from = "2000-01-01",
    to = Sys.Date(),
    determinands = NULL,
    quiet = FALSE,
    fail_fast = FALSE,
    workers = NULL
) {

  from <- parse_wims_date(from)
  to <- parse_wims_date(to)

  if (!is.character(sites)) {
    stop("'sites' must be a character vector")
  }

  if (length(sites) == 0) {
    stop("'sites' cannot be empty")
  }

  sites <- unique(stats::na.omit(sites))

  if (length(sites) == 0) {
    stop("No valid sites supplied")
  }

  if (!is.null(workers)) {
    future::plan(
      future::multisession,
      workers = workers
    )

    on.exit(
      future::plan(future::sequential),
      add = TRUE
    )
  }

  results <- furrr::future_map(
    sites,

    function(site) {

      if (!quiet) {
        message("Downloading ", site)
      }

      tryCatch(

        get_site(
          sample_point_id = site,
          from = from,
          to = to,
          determinands = determinands
        ),

        error = function(e) {

          msg <- paste(
            "Failed:",
            site,
            "-",
            conditionMessage(e)
          )

          if (fail_fast) {
            stop(msg)
          }

          warning(msg)

          NULL
        }
      )
    },

    .options = furrr::furrr_options(
      seed = TRUE
    )
  )

  results <- purrr::compact(results)

  successful <- length(results)
  attempted <- length(sites)

  if (!quiet) {
    message(
      successful,
      " of ",
      attempted,
      " sites downloaded successfully."
    )
  }

  if (successful == 0) {

    warning(
      "No sites were successfully downloaded."
    )

    return(
      empty_observations()
    )
  }

  dplyr::bind_rows(results) |>
    dplyr::arrange(sample_date)
}
