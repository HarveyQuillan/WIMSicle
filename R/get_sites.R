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
    determinands = NULL
) {

  from <- parse_wims_date(from)
  to <- parse_wims_date(to)

  if (!is.character(sites)) {
    stop("'sites' must be a character vector")
  }

  if (length(sites) == 0) {
    stop("'sites' cannot be empty")
  }

  if (!quiet) {
    message("Downloading ", site)
  }

  purrr::map_dfr(
    sites,
    function(site) {

      message(
        "Downloading ",
        site
      )

      tryCatch(

        get_site(
          sample_point_id = site,
          from = from,
          to = to,
          determinands = determinands
        ),

        error = function(e) {

          warning(
            paste(
              "Failed:",
              site,
              "-",
              e$message
            )
          )

          tibble::tibble()

        }
      )
    }
  )

}
