#' Get sampling points within a water body
#'
#' Downloads a water body boundary from the Environment Agency
#' Catchment Planning API and returns sampling points that fall within
#' the water body polygon.
#'
#' The function automatically:
#' \itemize{
#'   \item downloads the water body geometry;
#'   \item calculates a search area around the water body;
#'   \item retrieves nearby sampling points from the Environment Agency
#'         Water Quality API;
#'   \item performs a spatial join to identify sampling points located
#'         within the water body boundary.
#' }
#'
#' Optional filters can be used to restrict results to specific
#' sampling point statuses or sampling point types.
#'
#' @param waterbody_id Character. Environment Agency water body
#'   identifier.
#' @param status_code Optional sampling point status code.
#' @param type_code Optional sampling point type code.
#'
#' @return
#' A tibble containing sampling point metadata for all sampling points
#' located within the specified water body.
#'
#' The returned data contains the same columns as
#' `get_sampling_points()`:
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
#' If no matching sampling points are found, an empty tibble is
#' returned.
#'
#' @examples
#' # All sampling points within a water body
#' get_waterbody_sites(
#'   waterbody_id = "GB106039023232"
#' )
#'
#' # Open sampling points only
#' get_waterbody_sites(
#'   waterbody_id = "GB106039023232",
#'   status_code = "O"
#' )
#'
#' # Open river monitoring sites only
#' get_waterbody_sites(
#'   waterbody_id = "GB106039023232",
#'   status_code = "O",
#'   type_code = "F6"
#' )
#'
#' @seealso
#' \code{\link{get_sampling_points}},
#' \code{\link{get_site}},
#' \code{\link{get_sites}}
#'
#' @export
get_waterbody_sites <- function(
    waterbody_id,
    status_code = NULL,
    type_code = NULL
) {

  if (
    is.null(waterbody_id) ||
    !is.character(waterbody_id) ||
    length(waterbody_id) != 1
  ) {

    stop(
      "'waterbody_id' must be a single character value."
    )

  }

  message(
    "Downloading water body geometry..."
  )

  waterbody_url <- paste0(
    "https://environment.data.gov.uk/catchment-planning/WaterBody/",
    waterbody_id,
    ".geojson"
  )

  waterbody <- sf::read_sf(waterbody_url, quiet = TRUE)

  waterbody <- sf::st_make_valid(waterbody)

  if (nrow(waterbody) == 0) {
    stop(
      "Waterbody not found"
    )
  }

  waterbody <- sf::st_transform(
    waterbody,
    27700
  )

  centroid <- sf::st_centroid(
    sf::st_union(waterbody)
  )

  coords <- sf::st_coordinates(
    centroid
  )

  easting <- round(
    coords[1, "X"]
  )

  northing <- round(
    coords[1, "Y"]
  )

  radius_m <- max(
    as.numeric(
      sf::st_distance(
        centroid,
        sf::st_cast(
          waterbody,
          "MULTILINESTRING"
        )
      )
    )
  )

  radius_km <- ceiling(
    radius_m / 1000
  )

  radius_km <- radius_km + 10

  message(
    "Downloading sampling points..."
  )

  sites <- get_sampling_points(
    easting = easting,
    northing = northing,
    radius = radius_km,
    status_code = status_code,
    type_code = type_code
  )

  if (nrow(sites) == 0) {

    message(
      "No sampling points found."
    )

    return(sites)

  }

  sites_sf <- sf::st_as_sf(
    sites,
    coords = c(
      "easting",
      "northing"
    ),
    crs = 27700
  )

  if (
    sf::st_crs(waterbody) !=
    sf::st_crs(sites_sf)
  ) {

    waterbody <- sf::st_transform(
      waterbody,
      sf::st_crs(sites_sf)
    )

  }

  message(
    "Performing spatial join..."
  )

  within_matrix <- sf::st_within(
    sites_sf,
    waterbody,
    sparse = FALSE
  )

  sites_sf <- sites_sf[
    apply(
      within_matrix,
      1,
      any
    ),
  ]

  message(
    nrow(sites_sf),
    " sampling points found."
  )

  sf::st_drop_geometry(
    sites_sf
  )

}
