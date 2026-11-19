# Find CDC HIV-related service providers by ZIP code

Queries the CDC Locator API to return service providers within a given
radius of one or more ZIP codes.

## Usage

``` r
cdc_providers(
  zipcode,
  radius = 10,
  services = NULL,
  client_id = NULL,
  client_token = NULL,
  distinct = TRUE,
  min_gap = NULL,
  progress = TRUE
)
```

## Arguments

- zipcode:

  One or more five-digit ZIP codes as strings, such as `"02110"`. A
  number would lose leading zeros, so it is rejected.

- radius:

  Search radius in miles: a whole number from 1 to 50. The default is
  10.

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

- distinct:

  With several ZIP codes, `TRUE` (the default) keeps each provider once,
  from the search where it was nearest. `FALSE` keeps one row for each
  ZIP code that found it.

- min_gap:

  The least time between requests, in seconds, from 0.5 to 60. `NULL`,
  the default, means 0.5 for one ZIP code and 1 for several.

- progress:

  With several ZIP codes, whether to show a progress bar (only in
  interactive sessions) and a one-line summary at the end.

## Value

A tibble of service providers, with columns `type`, `id`,
`search_zipcode` (the ZIP code whose search found the provider) and
`attributes`. There is one row per provider record, or, for several ZIP
codes with `distinct = FALSE`, one row per provider record and ZIP code
that found it. Use
[`cdc_providers_tidy()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers_tidy.md)
to flatten it.

## Details

The API returns providers offering any of the requested services, but
returned no providers when all 14 services were requested at once
(observed 2026-11-16). So services are requested at most three at a
time, and the results are combined: the default, every service, takes
five requests per ZIP code. Providers returned by more than one request
for the same ZIP code are kept once, by `id`.

## Several ZIP codes

Give `zipcode` a vector to search around each ZIP code in turn. Every
ZIP code is checked before any request is sent, and a repeated ZIP code
is searched once. Nearby circles overlap, so with `distinct = TRUE` a
provider found from more than one ZIP code is kept once, from the search
where it was nearest; its `distance` and `search_zipcode` then belong
together.

With one ZIP code, an error stops the call. With several, a ZIP code the
API doesn't know (HTTP 500 "No point found for address") is skipped, the
search carries on, and a warning lists the skipped ZIP codes at the end.
They are also kept, with the API's message, in
`attr(result, "failures")`; read it straight away, because most
data-frame operations, including
[`cdc_providers_tidy()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers_tidy.md),
drop it. Any other error, such as a refused request (HTTP 403), a
gateway error (502 or 504), any other HTTP 500 or a network failure,
stops the call, because it could affect every remaining ZIP code.

Requests are spaced at least `min_gap` seconds apart: 0.5 seconds for
one ZIP code and 1 second for several, unless you set it. A search of 30
ZIP codes for every service is 150 requests, about two and a half
minutes. In an interactive session a progress bar shows how far the
search has got. Pressing Esc stops the search and discards what was
found so far, so for a long search it can help to search in smaller
groups of ZIP codes.
