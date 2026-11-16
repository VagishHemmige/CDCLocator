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

A tibble with one row per provider and columns `provider_type`,
`provider_id`, `name`, `city`, `country`, `state`, `street`, `zipcode`,
`categories`, `contact_others`, `contact_telephone`, `contact_website`,
`languages` (a list of character vectors), `latitude`, `longitude` and
`distance` (miles from the searched ZIP code). When no providers
matched, the tibble has these columns and zero rows.
