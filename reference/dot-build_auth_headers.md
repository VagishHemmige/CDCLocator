# Build authentication headers for the CDC Locator API

Credentials are discovered in the following order:

1.  Explicit arguments

2.  Environment variables:

    - CDC_LOCATOR_CLIENT_ID

    - CDC_LOCATOR_CLIENT_TOKEN

## Usage

``` r
.build_auth_headers(client_id = NULL, client_token = NULL)
```

## Arguments

- client_id:

  CDC client ID (optional)

- client_token:

  CDC client token (optional)

## Value

Named character vector suitable for httr2::req_headers()
