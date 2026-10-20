# ------ Internal helper functions ------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ------ Parsing helpers ------

parse_observation <- function(obs) {

  tibble::tibble(
    sampling_point =
      obs$hasSamplingPoint$notation %||% NA_character_,

    site_name =
      obs$hasSamplingPoint$prefLabel %||% NA_character_,

    sample_date =
      obs$phenomenonTime %||% NA_character_,

    determinand_code =
      obs$observedProperty$notation %||% NA_character_,

    determinand =
      obs$observedProperty$prefLabel %||% NA_character_,

    result =
      suppressWarnings(
        as.numeric(
          obs$hasResult$numericValue %||%
            obs$hasResult$literalValue %||%
            obs$hasResult$prefLabel %||%
            NA_character_
        )
      ),

    result_raw =
      as.character(
        obs$hasResult$numericValue %||%
          obs$hasResult$literalValue %||%
          obs$hasResult$prefLabel %||%
          NA_character_
      ),

    unit =
      obs$hasResult$hasUnit$altLabel %||% NA_character_,

    sample_material =
      obs$hasSample$sampleMaterialType$prefLabel %||%
      NA_character_,

    sampling_purpose =
      obs$hasSample$isResultOf$samplingPurpose$prefLabel %||%
      NA_character_
  )

}

parse_determinand <- function(x) {

  tibble::tibble(
    determinand_code =
      x$notation %||% NA_character_,

    determinand =
      x$prefLabel %||% NA_character_
  )

}

parse_wims_date <- function(x) {

  if (missing(x) || is.null(x)) {
    return(as.character(Sys.Date()))
  }

  if (length(x) == 0) {
    return(as.character(Sys.Date()))
  }

  if (all(is.na(x))) {
    return(as.character(Sys.Date()))
  }

  x <- as.character(x)

  if (identical(x, "")) {
    return(as.character(Sys.Date()))
  }

  if (grepl("^\\d{4}$", x)) {
    return(paste0(x, "-01-01"))
  }

  if (grepl("^\\d{4}-\\d{2}$", x)) {
    return(paste0(x, "-01"))
  }

  date <- as.Date(x)

  if (is.na(date)) {
    stop(
      "Invalid date: ",
      x
    )
  }

  as.character(date)

}

# ------ Determinand helpers ------

extract_determinands <- function(data) {

  data |>
    dplyr::distinct(
      determinand_code,
      determinand
    ) |>
    dplyr::arrange(determinand)

}

find_best_determinand <- function(
    search,
    determinand_df
) {

  search <- tolower(search)

  determinand_df |>
    dplyr::mutate(
      det_lower = tolower(determinand),

      score = dplyr::case_when(
        det_lower == search ~ 1,
        startsWith(det_lower, search) ~ 2,
        grepl(
          paste0("\\b", search, "\\b"),
          det_lower
        ) ~ 3,
        grepl(
          search,
          det_lower
        ) ~ 4,
        TRUE ~ 999
      )
    ) |>
    dplyr::filter(
      score < 999
    ) |>
    dplyr::arrange(score)

}

resolve_determinands <- function(
    determinands,
    site_dets
) {

  resolved <- character()

  for (det in determinands) {

    if (grepl(
      "^[0-9]+$",
      det
    )) {

      resolved <- c(
        resolved,
        det
      )

      next

    }

    matches <- find_best_determinand(
      det,
      site_dets
    )

    if (nrow(matches) == 0) {

      warning(
        paste(
          "No determinand match found for",
          det
        )
      )

      next

    }

    best_match <- matches[1, ]

    message(
      "Matched '",
      det,
      "' -> '",
      best_match$determinand,
      "' (",
      best_match$determinand_code,
      ")"
    )

    resolved <- c(
      resolved,
      best_match$determinand_code
    )

  }

  unique(resolved)

}

# ------ Sampling point helpers ------

parse_sampling_point <- function(x) {

  wkt <- x$geometry$asWKT %||% NA_character_

  coords <- regmatches(
    wkt,
    regexec(
      "POINT\\(([0-9\\.]+) ([0-9\\.]+)\\)",
      wkt
    )
  )[[1]]

  easting <- if (length(coords) >= 2) {
    as.numeric(coords[2])
  } else {
    NA_real_
  }

  northing <- if (length(coords) >= 3) {
    as.numeric(coords[3])
  } else {
    NA_real_
  }

  tibble::tibble(
    sampling_point =
      x$notation %||% NA_character_,

    site_name =
      x$prefLabel %||% NA_character_,

    status =
      x$samplingPointStatus$prefLabel %||%
      NA_character_,

    status_code =
      x$samplingPointStatus$notation %||%
      NA_character_,

    type =
      x$samplingPointType$prefLabel %||%
      NA_character_,

    type_code =
      x$samplingPointType$notation %||%
      NA_character_,

    region =
      x$region$prefLabel %||%
      NA_character_,

    region_code =
      x$region$notation %||%
      NA_character_,

    area =
      x$area$prefLabel %||%
      NA_character_,

    area_code =
      x$area$notation %||%
      NA_character_,

    sub_area =
      x$subArea$prefLabel %||%
      NA_character_,

    sub_area_code =
      x$subArea$notation %||%
      NA_character_,

    easting = easting,

    northing = northing
  )

}

# ------ API helpers ------

perform_wims_request <- function(req) {

  tryCatch(

    req |>
      httr2::req_retry(
        max_tries = 5
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_perform(),

    error = function(e) {

      stop(
        "WIMS API request failed: ",
        conditionMessage(e),
        call. = FALSE
      )

    }
  )
}


empty_observations <- function() {

  tibble::tibble(
    sampling_point = character(),
    site_name = character(),
    sample_date = as.POSIXct(
      character(),
      tz = "UTC"
    ),
    determinand_code = character(),
    determinand = character(),
    result = numeric(),
    result_raw = character(),
    unit = character(),
    sample_material = character(),
    sampling_purpose = character()
  )

}

empty_sampling_points <- function() {
  tibble::tibble()
}

validate_date_range <- function(from, to) {

  if (as.Date(from) > as.Date(to)) {
    stop(
      "'from' must be before 'to'"
    )
  }

}
