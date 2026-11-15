# Translate city and state to a ZIP code

Uses the CDC Locator API Place Translator endpoint to convert a city and
state into a ZIP code.

## Usage

``` r
cdc_address_place(city, state, client_id = NULL, client_token = NULL)
```

## Arguments

- city:

  Character scalar. City name.

- state:

  Character scalar. State name or postal abbreviation.

- client_id:

  Optional CDC client ID override.

- client_token:

  Optional CDC client token override.

## Value

A tibble with city, state, zipcode, latitude, and longitude.
