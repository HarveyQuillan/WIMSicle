# WIMSicle

WIMSicle provides tools for downloading, exploring and visualising Environment Agency water quality data using the Water Quality API.

## Features

- Download observations for a single monitoring site
- Download observations for multiple monitoring sites
- Convert non WIMS data into WIMS format
- Retrieve and filter sampling point metadata
- Search available determinands
- Retrieve determinands available at a site
- Create dual-axis time series plots
- Return tidy data frames ready for analysis

## Installation

``` r
devtools::install("path/to/WIMSicle")
```

## Load the package

``` r
library(WIMSicle)
```

## Download data for a site

``` r
data <- get_site(
  sample_point_id = "TH-PCNR0051",
  from = "2020-01-01",
  to = "2021-01-01"
)
```

## Find determinands

``` r
find_determinand("nitrate")
```

Example output:

``` r
# A tibble: ...
  determinand_code determinand
  <chr>            <chr>
1 0061             Nitrate-N
```

## Get site determinands

``` r
get_site_determinands(
  sample_point_id = "TH-PCNR0051",
  from = "2020-01-01",
  to = "2021-01-01"
)
```

## Get sampling points

Download sampling point metadata and filter by region, area, sub-area, type, status, notation, or site name.

### Download all sampling points

``` r
sites <- get_sampling_points()
```

### Filter by region

``` r
sites <- get_sampling_points(
  region_code = "TH"
)
```

### Filter by area

``` r
sites <- get_sampling_points(
  region_code = "TH",
  area_code = "M"
)
```

### Filter by site type

``` r
sites <- get_sampling_points(
  type_code = "UA"
)
```

### Search by site name

``` r
sites <- get_sampling_points(
  site_name = "Mogden"
)
```

### Retrieve a specific site

``` r
site <- get_sampling_points(
  point_notation = "TH-PCNR0051"
)
```

## Download data for all sites in an area

One of the most powerful workflows is using `get_sampling_points()` to select sites and then passing the results directly to `get_sites()`.

### Example: Download all sites in a region

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

### Example: Download all sites in an area

``` r
sites <- get_sampling_points(
  region_code = "TH",
  area_code = "M"
)

data <- get_sites(
  sites = sites$sampling_point,
  from = "2020",
  to = "2025"
)
```

### Example: Download all sewage discharge sites

``` r
sites <- get_sampling_points(
  type_code = "UA"
)

data <- get_sites(
  sites = sites$sampling_point,
  from = "2020",
  to = "2025"
)
```

## Download multiple sites

``` r
data <- get_sites(
  sites = c(
    "TH-PCNR0051",
    "TH-PCNR0052"
  ),
  from = "2020-01-01",
  to = "2021-01-01"
)
```

## Plot two determinands

``` r
plot_data <- get_site(
  sample_point_id = "TH-PCNR0051",
  from = "2010-01-01",
  to = "2015-01-01",
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

## Returned Data

### Observation Data

The package returns tibbles with the following columns:

| Column           | Description          |
|------------------|----------------------|
| sampling_point   | Sampling point ID    |
| site_name        | Site name            |
| sample_date      | Sample date and time |
| determinand_code | Determinand code     |
| determinand      | Determinand name     |
| result           | Observed value       |
| unit             | Measurement unit     |
| sample_material  | Sample material      |
| sampling_purpose | Sampling purpose     |

### Sampling Point Data

`get_sampling_points()` returns:

| Column         | Description                    |
|----------------|--------------------------------|
| sampling_point | Sampling point ID              |
| site_name      | Site name                      |
| status         | Sampling point status          |
| status_code    | Status code                    |
| type           | Sampling point type            |
| type_code      | Type code                      |
| region         | Region name                    |
| region_code    | Region code                    |
| area           | Area name                      |
| area_code      | Area code                      |
| sub_area       | Sub-area name                  |
| sub_area_code  | Sub-area code                  |
| easting        | British National Grid easting  |
| northing       | British National Grid northing |

## WIMSify Your Own Data

`wimsify()` allows non-WIMS data to be converted into the standard WIMSicle format so it can be used with WIMSicle plotting and analysis functions.

### Basic Usage

Map your source columns to the WIMSicle schema:

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

The returned data can then be used with any WIMSicle plotting function.

``` r
plot_dual_determinand(
  wq_data,
  determinand_1 = "Ammonia",
  determinand_2 = "Orthophosphate"
)
```

### Standardising Determinand Names

Many laboratory and operational datasets use different naming conventions to WIMS.

For example:

| Source Name    | WIMS Equivalent               |
|----------------|-------------------------------|
| P SOL Reactive | Orthophosphate, reactive as P |
| NH3-N          | Ammoniacal-N                  |
| NO3-N          | Nitrate-N                     |

A custom mapping table can be supplied to automatically translate source names into official WIMS determinands.

``` r
det_map <- tibble::tribble(
  ~source_name,      ~determinand_code,
  "P SOL Reactive",  "0180",
  "NH3-N",           "0111",
  "NO3-N",           "0061"
)
```

``` r
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

WIMSicle will:

- Preserve the original determinand name
- Add the relevant WIMS determinand code
- Replace the determinand name with the official WIMS determinand name

For example:

| Original Determinand | Determinand Code | WIMS Determinand              |
|----------------------|------------------|-------------------------------|
| P SOL Reactive       | 0180             | Orthophosphate, reactive as P |

### Typical Workflow

``` r
raw_data <- read.csv("lab_results.csv")

det_map <- tibble::tribble(
  ~source_name,      ~determinand_code,
  "P SOL Reactive",  "0180",
  "NH3-N",           "0111"
)

wq_data <- wimsify(
  data = raw_data,
  site_name = "Site",
  sample_date = "Date",
  determinand = "Parameter",
  result = "Result",
  determinand_map = det_map
)

plot_dual_determinand(
  wq_data,
  determinand_1 = "Orthophosphate, reactive as P",
  determinand_2 = "Ammoniacal-N"
)
```

This allows data from other sources to be analysed using the same workflow as data downloaded directly from the Environment Agency Water Quality API.

## Development Status

WIMSicle is currently under active development and interfaces with the Environment Agency Water Quality API.

Please direct questions, bugs and feature ideas to harvey.q.goodman\@gmail.com
