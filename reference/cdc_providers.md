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
  `"housing"`. `NULL`, the default, searches for all of them.

- client_id:

  Optional CDC client ID override.

- client_token:

  Optional CDC client token override.

## Value

A tibble of service providers.
