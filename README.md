# WIMSicle

(https://github.com/HarveyQuillan/WIMSicle/actions/workflows/R-CMD-check.yaml/badge.svg)

WIMSicle provides tools for downloading, exploring and visualising Environment Agency water quality data using the Water Quality API.

## Features

- Download observations from a single monitoring site
- Download observations from multiple monitoring sites
- Retrieve and filter sampling point metadata
- Discover available determinands
- Find determinands available at a site
- Convert external datasets into WIMS-compatible format
- Create dual-axis time-series plots
- Return tidy tibbles ready for analysis

## Installation

### From GitHub

``` r
remotes::install_github(
  "HarveyQuillan/WIMSicle"
)
```

### Load the package

``` r
library(WIMSicle)
```

## Quick Start

### Download observations

``` r
data <- get_site(
  sample_point_id = "TH-PCNR0051",
  from = "2020-01-01",
  to = "2021-01-01"
)
```

### Search determinands

``` r
find_determinand("nitrate")
```

### Get available determinands

``` r
get_site_determinands(
  sample_point_id = "TH-PCNR0051"
)
```

### Download sampling point metadata

``` r
sites <- get_sampling_points(
  region_code = "TH"
)
```

### Download multiple sites

``` r
data <- get_sites(
  sites = c(
    "TH-PCNR0051",
    "TH-PNAR0250"
  )
)
```

## Working with Sampling Points

### Find a specific site

``` r
get_sampling_points(
  point_notation = "TH-PCNR0051"
)
```

### Search by site name

``` r
get_sampling_points(
  site_name = "Mogden"
)
```

### Filter by site type

``` r
get_sampling_points(
  type_code = "UA"
)
```

### Search by location

``` r
get_sampling_points(
  easting = 530000,
  northing = 180000,
  radius = 10
)
```

## Download Data for Many Sites

``` r
sites <- get_sampling_points(
  region_code = "TH"
)

data <- get_sites(
  sites = sites$sampling_point,
  from = "2020",
  to = "2025"
)
```

## Plot Two Determinands

``` r
plot_data <- get_site(
  sample_point_id = "TH-PCNR0051",
  determinands = c(
    "Nitrate",
    "Chlorophyll"
  )
)

plot_dual_determinand(
  plot_data,
  determinand_1 = "Nitrate",
  determinand_2 = "Chlorophyll"
)
```

## Convert Data with wimsify()

``` r
wq_data <- wimsify(
  data = my_data,
  site_name = "Site",
  sample_date = "Date",
  determinand = "Parameter",
  result = "Result",
  unit = "Units"
)
```

### Apply a determinand mapping

``` r
det_map <- tibble::tribble(
  ~source_name,     ~determinand_code,
  "P SOL Reactive", "0180",
  "NH3-N",          "0111",
  "NO3-N",          "0061"
)

wq_data <- wimsify(
  data = lab_data,
  site_name = "Site",
  sample_date = "Sample Date",
  determinand = "Parameter",
  result = "Result",
  unit = "Unit",
  determinand_map = det_map
)
```

This will:

- Preserve the original determinand name
- Add the WIMS determinand code
- Apply the official Environment Agency determinand name

## Returned Data

### Observation data

| Column           | Description                         |
|------------------|-------------------------------------|
| sampling_point   | Sampling point identifier           |
| site_name        | Site name                           |
| sample_date      | Sample date and time                |
| determinand_code | Environment Agency determinand code |
| determinand      | Determinand name                    |
| result           | Numeric result                      |
| result_raw       | Original result value               |
| unit             | Measurement unit                    |
| sample_material  | Sample material                     |
| sampling_purpose | Sampling purpose                    |

### Sampling point data

| Column         | Description                    |
|----------------|--------------------------------|
| sampling_point | Sampling point identifier      |
| site_name      | Site name                      |
| status         | Sampling point status          |
| status_code    | Status code                    |
| type           | Sampling point type            |
| type_code      | Type code                      |
| region         | Region                         |
| region_code    | Region code                    |
| area           | Area                           |
| area_code      | Area code                      |
| sub_area       | Sub-area                       |
| sub_area_code  | Sub-area code                  |
| easting        | British National Grid easting  |
| northing       | British National Grid northing |

## Licence

MIT License.

## Development Status

WIMSicle is under active development.

Issues, bug reports and feature requests are welcome via GitHub Issues.
