# Translate latitude and longitude to a ZIP code

Uses the CDC Locator API Point Translator endpoint to convert geographic
coordinates into a ZIP code.

## Usage

``` r
cdc_address_point(latitude, longitude, client_id = NULL, client_token = NULL)
```

## Arguments

- latitude:

  Latitude in decimal degrees, from -90 to 90.

- longitude:

  Longitude in decimal degrees, from -180 to 180. Places in the US have
  negative longitudes.

- client_id:

  Optional CDC client ID override.

- client_token:

  Optional CDC client token override.

## Value

A tibble with city, state, zipcode, latitude, and longitude.
