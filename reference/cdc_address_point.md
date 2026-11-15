# Translate latitude and longitude to a ZIP code

Uses the CDC Locator API Point Translator endpoint to convert geographic
coordinates into a ZIP code.

## Usage

``` r
cdc_address_point(latitude, longitude, client_id = NULL, client_token = NULL)
```

## Arguments

- latitude:

  Numeric latitude.

- longitude:

  Numeric longitude.

- client_id:

  Optional CDC client ID override.

- client_token:

  Optional CDC client token override.

## Value

A tibble with city, state, zipcode, latitude, and longitude.
