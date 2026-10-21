#' Download water quality observations for multiple sampling points
#'
#' Retrieves water quality observations for multiple sampling points and
#' combines the results into a single tibble.
#'
#' The function calls `get_site()` for each supplied sampling point and
#' can optionally download sites in parallel using multiple workers.
#' Failed site downloads can either generate warnings and continue, or
#' stop execution immediately.
#'
#' @param sites Character vector of Environment Agency sampling point
#'   identifiers.
#' @param from Character or Date. Start date for the search period.
#'   Partial dates such as `"2020"` or `"2020-01"` are accepted.
#' @param to Character or Date. End date for the search period.
#'   Partial dates such as `"2020"` or `"2020-12"` are accepted.
#' @param determinands Optional character vector of determinand names
#'   and/or determinand codes.
#' @param quiet Logical. If `TRUE`, suppresses progress messages.
#' @param fail_fast Logical. If `TRUE`, stop immediately when a site
#'   download fails. If `FALSE`, failed sites generate warnings and
#'   processing continues.
#' @param workers Optional integer specifying the number of parallel
#'   workers to use. If `NULL`, processing occurs sequentially.
#'
#' @return
#' A tibble containing observations from all successfully downloaded
#' sites.
#'
#' The returned data contains the same columns as `get_site()`:
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
#' Results are returned in chronological order. If no sites are
#' successfully downloaded, an empty observations tibble is returned.
#'
#' @examples
#' # Download observations from multiple sites
#' get_sites(
#'   sites = c(
#'     "TH-PCNR0051",
#'     "TH-PNAR0250"
#'   )
#' )
#'
#' # Restrict by date range
#' get_sites(
#'   sites = c(
#'     "TH-PCNR0051",
#'     "TH-PNAR0250"
#'   ),
#'   from = "2020-01-01",
#'   to = "2020-12-31"
#' )
#'
#' # Filter by determinand
#' get_sites(
#'   sites = c(
#'     "TH-PCNR0051",
#'     "TH-PNAR0250"
#'   ),
#'   determinands = "Nitrate"
#' )
#'
#' # Download using parallel workers
#' get_sites(
#'   sites = c(
#'     "TH-PCNR0051",
#'     "TH-PNAR0250"
#'   ),
#'   workers = 4
#' )
#'
#' @seealso
#' \code{\link{get_site}},
#' \code{\link{get_site_determinands}},
#' \code{\link{get_sampling_points}}
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
