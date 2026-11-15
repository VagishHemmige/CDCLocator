# Build an httr2 request for the CDC Locator API

This is the single entry point for all API requests in the package.

## Usage

``` r
.cdc_locator_request(
  endpoint,
  query = list(),
  client_id = NULL,
  client_token = NULL
)
```

## Arguments

- endpoint:

  Character scalar. API endpoint path (e.g. "providers").

- query:

  Named list of query parameters.

- client_id:

  Optional CDC client ID override.

- client_token:

  Optional CDC client token override.

## Value

An httr2 request object.
