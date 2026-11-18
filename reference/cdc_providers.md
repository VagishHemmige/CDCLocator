# Find CDC HIV-related service providers by ZIP code

Queries the CDC Locator API to return service providers within a given
radius of a ZIP code.

## Usage

``` r
cdc_providers(
  zipcode,
  radius = 10,
  services = NULL,
  client_id = NULL,
  client_token = NULL
)
```

## Arguments

- zipcode:

  Character scalar. ZIP code.

- radius:

  Integer search radius in miles (1–50, default: 10).

- services:

  Character vector of services to search for, matched in any case:
  `"hivtesting"`, `"stdtesting"`, `"selftesting"`, `"preptesting"`,
  `"pep"`, `"condom"`, `"hepc"`, `"doxypep"`, `"mailintesting"`,
  `"ryanwhite"`, `"clinics"`, `"substanceabuse"`, `"mentalhealth"` or
  `"housing"`. `NULL`, the default, searches for all of them. More than
  three services take more than one request.

- client_id:

  Optional CDC client ID override.

- client_token:

  Optional CDC client token override.

## Value

A tibble of service providers.

## Details

The API returns providers offering any of the requested services, but
returned no providers when all 14 services were requested at once
(observed 2026-11-16). So services are requested at most three at a
time, and the results are combined: the default, every service, takes
five requests. Providers returned by more than one request are kept
once, by `id`.
