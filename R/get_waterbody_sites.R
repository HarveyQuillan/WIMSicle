#' Get sampling points within a water body
#'
#' Downloads a water body polygon from the Environment Agency
#' Catchment Planning API and returns all sampling points that
#' fall within the water body.
#'
#' @param waterbody_id Water body identifier.
#' @param status_code Optional sampling point status code.
#' @param type_code Optional sampling point type code.
#'
#' @return A tibble of sampling points within the water body.
#'
#' @examples
#' sites <- get_waterbody_sites(
#'   "GB106039023232"
#' )
#'
#' sites <- get_waterbody_sites(
#'   "GB106039023232",
#'   status_code = "O"
#' )
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

  waterbody <- sf::st_make_valid(
    waterbody,
    quiet = TRUE
  )

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
