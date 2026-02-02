# Multiply emission factors by distances

Calculate hot exhaust emissions by multiplying emission factors by
distances weighted by fleet composition profile.

## Usage

``` r
multiply_ef(
  fleet_composition,
  dist,
  ef,
  aggregate = TRUE,
  prefix = NULL,
  as_list = TRUE
)
```

## Arguments

- fleet_composition:

  vector; Fleet composition, which is a distribution of fleet based on
  frequency. If there is only one, 'fleet_composition' is 1.0.

- dist:

  units ('km'); Length of each link in km.

- ef:

  list or data.table; Emission factors.

- aggregate:

  logical; if TRUE (default) emissions are aggregated by pollutant.

- prefix:

  character; Add prefix into emissions names. Missing parameter
  (default) means empty prefix.

- as_list:

  logical. If `TRUE` (default), the function returns the output in a
  `list` format. If `FALSE`, the output is returned in a `data.table`
  format.

## Value

units ('g'); emissions per link.
