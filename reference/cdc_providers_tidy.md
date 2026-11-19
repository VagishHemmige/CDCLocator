# Tidy CDC Locator provider output

Flattens nested provider attributes into an analysis-ready tibble.

## Usage

``` r
cdc_providers_tidy(providers)
```

## Arguments

- providers:

  Output from cdc_providers()

## Value

A tibble with one row for each row of `providers` (one per provider
record, or with `cdc_providers(distinct = FALSE)` one per provider
record and ZIP code that found it) and columns `provider_type`,
`provider_id`, `search_zipcode` (the ZIP code whose search found the
provider), `name`, `city`, `country`, `state`, `street`, `zipcode`,
`categories`, `contact_others`, `contact_telephone`, `contact_website`,
`languages` (a list of character vectors), `latitude`, `longitude` and
`distance` (miles from the searched ZIP code). When no providers
matched, the tibble has these columns and zero rows.
