# Services and searching

``` r

library(CDCLocator)
```

This article explains what
[`cdc_providers()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers.md)
can search for and how a search works: the 14 services, the radius, how
services combine, and why a search can take several requests.

## The search

``` r

cdc_providers(zipcode, radius = 10, services = NULL)
```

- **`zipcode`**: a five-digit ZIP code as a string, such as `"02110"`.
  The API searches around that ZIP code’s location. It doesn’t accept a
  city, an address or coordinates; to start from those, see “Searching
  from a place or a point” below.
- **`radius`**: how far to search, in miles. A whole number from 1 to
  50; the default is 10.
- **`services`**: which kinds of provider to look for. `NULL`, the
  default, means all 14.

## The 14 services

These are the values `services` accepts. Case doesn’t matter. The agency
column says which federal program the listings come from, according to
HIV.gov’s instructions for the API.

| Value | Covers | Agency |
|----|----|----|
| `hivtesting` | HIV testing | CDC |
| `stdtesting` | Testing for sexually transmitted infections | CDC |
| `selftesting` | Self-administered HIV and STI testing | CDC |
| `preptesting` | Pre-exposure prophylaxis (PrEP) providers | CDC |
| `pep` | Post-exposure prophylaxis (PEP) providers | CDC |
| `condom` | Free or low-cost condoms | CDC |
| `hepc` | Hepatitis C testing | CDC |
| `doxypep` | Doxycycline post-exposure prophylaxis (doxy-PEP) | CDC |
| `mailintesting` | Mail-in-only HIV self-testing kits | CDC |
| `ryanwhite` | Ryan White HIV/AIDS Program primary care, services and medications | HRSA |
| `clinics` | HRSA-funded health centers | HRSA |
| `substanceabuse` | State-licensed substance use disorder treatment | SAMHSA |
| `mentalhealth` | State-licensed mental health treatment | SAMHSA |
| `housing` | Help finding low-income affordable housing | HUD |

CDC is the Centers for Disease Control and Prevention, HRSA the Health
Resources and Services Administration, SAMHSA the Substance Abuse and
Mental Health Services Administration, and HUD the Department of Housing
and Urban Development.

HIV.gov’s instructions spell hepatitis C as `Hepc`, but in November 2026
the API only matched `hepc`. The package sends every value in lower
case, so either spelling works.

The public website at locator.hiv.gov shows some categories that aren’t
on this list, such as tuberculosis testing. The API may not offer those.

## How services combine

Naming several services finds providers that offer **any** of them, not
all of them:

``` r

cdc_providers("20176", radius = 10, services = c("hivtesting", "stdtesting"))
```

returns everyone who offers HIV testing, plus everyone who offers STI
testing.

To find providers that offer both, search for each and keep the ones
that appear in both. Because the same clinic can have a different record
for each service (see below), compare names and addresses rather than
`id`:

``` r

hiv <- cdc_providers_tidy(
  cdc_providers("20176", radius = 10, services = "hivtesting")
)
sti <- cdc_providers_tidy(
  cdc_providers("20176", radius = 10, services = "stdtesting")
)
both <- merge(
  unique(hiv[, c("name", "street", "zipcode")]),
  unique(sti[, c("name", "street", "zipcode")])
)
both
```

## Why a search can take several requests

When asked for all 14 services in one request, the API returned no
providers at all, even where single-service searches found dozens.
Requests with up to three services worked. So the package sends services
**three at a time** and combines the answers:

| Services asked for     | Requests |
|------------------------|----------|
| 1 to 3                 | 1        |
| 4 to 6                 | 2        |
| 7 to 9                 | 3        |
| 10 to 12               | 4        |
| 13 or 14 (the default) | 5        |

Requests are spaced at least half a second apart, to go easy on the API,
so the default search takes a few seconds. If you only need some
services, naming them is faster.

If any of the requests fails, the whole search fails with that error.
You never get a partial result without knowing it.

Providers that come back from more than one request are kept once,
matched by their `id`.

## Duplicates across services

The API seems to keep a separate record for each service a place offers.
In November 2026, searches for HIV testing and STI testing near the same
ZIP code returned no shared `id`s, even though the same clinics appeared
in both.

So when you search for several services, **one clinic can appear several
times**, with different `id`s.
[`vignette("working-with-results")`](https://vagishhemmige.github.io/CDCLocator/articles/working-with-results.md)
shows how to collapse those rows by name and address when you want one
row per place.

## Searching from a place or a point

[`cdc_providers()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers.md)
only takes a ZIP code. Two functions translate other starting points:

``` r

cdc_address_place("Peoria", "IL")
cdc_address_point(40.69365, -89.58899)
```

[`cdc_address_place()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_address_place.md)
takes a city and a state, written as the postal code (`"IL"`) or in
full.
[`cdc_address_point()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_address_point.md)
takes a latitude and longitude in decimal degrees; places in the US have
negative longitudes. Both return the city, the full state name, the ZIP
code and its coordinates.

Then search from that ZIP code:

``` r

place <- cdc_address_place("Peoria", "IL")
cdc_providers(place$zipcode, radius = 5, services = "ryanwhite")
```

The search is centered on the ZIP code, not on the exact point you
started from, so `distance` in the results is measured from the ZIP
code’s location.

## Searching many ZIP codes

[`cdc_providers()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers.md)
takes one ZIP code at a time. For several, loop over them and bind the
results. Keep the number of requests in mind: each ZIP code needs one
request per three services.

``` r

zips <- c("20176", "20147", "20148")
results <- lapply(zips, \(zip) {
  cdc_providers(zip, radius = 5, services = "hivtesting") |>
    cdc_providers_tidy()
})
all_results <- dplyr::bind_rows(results)

# Nearby ZIP codes overlap, so the same records come back more than once.
all_results <- all_results[!duplicated(all_results$provider_id), ]
```

`distance` is measured from whichever ZIP code found the record first,
so it means less once results from several ZIP codes are combined.
