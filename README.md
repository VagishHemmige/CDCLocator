
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CDCLocator

<!-- badges: start -->

[![R-CMD-check](https://github.com/VagishHemmige/CDCLocator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/VagishHemmige/CDCLocator/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

CDCLocator finds HIV testing, prevention, care and related service
providers near a US ZIP code, using the HIV.gov Locator API. It returns
the results as tibbles you can filter, map or join to other data.

It can:

- search for providers within 1 to 50 miles of a ZIP code, for any of 14
  kinds of service, such as HIV testing, PrEP, Ryan White care or
  housing help;
- turn the nested provider records into one row per provider, with name,
  address, phone, website, languages, coordinates and distance;
- look up the ZIP code for a city and state, or for a latitude and
  longitude.

CDCLocator is not affiliated with or endorsed by HIV.gov. It is in early
development, and the API’s behavior described here was observed in
November 2026.

## Installation

Install the development version from GitHub, building the guides so you
can open them in R with `vignette()`:

``` r
# install.packages("remotes")
remotes::install_github(
  "VagishHemmige/CDCLocator",
  dependencies = TRUE,
  build_vignettes = TRUE
)
```

Building the guides needs Pandoc, which comes with RStudio. Without it,
or to install faster, leave out `build_vignettes = TRUE`: the package
works the same, and the guides are on the package website instead.

## Before you start: API credentials

The Locator API needs a client ID and a client token, which HIV.gov
issues on request. The [Locator page on
HIV.gov](https://www.hiv.gov/locator) says how to ask; in November 2026
it pointed to <contact@hiv.gov>.

Once you have them, save them in your `.Renviron` file so R finds them
each time it starts. The package can do this for you: it asks first,
names the file, and writes only after you say yes.

``` r
CDCLocator::cdc_locator_set_credentials(
  client_id = "your-client-id",
  client_token = "your-client-token",
  install = TRUE
)
```

Or add them yourself: `usethis::edit_r_environ()` opens the file. Add
these two lines, then restart R:

    CDC_LOCATOR_CLIENT_ID=your-client-id
    CDC_LOCATOR_CLIENT_TOKEN=your-client-token

Treat the token like a password: keep it out of scripts, and never
commit `.Renviron` to Git. [Credentials and
networks](https://vagishhemmige.github.io/CDCLocator/articles/credentials-and-networks.html)
has more detail, including how a project’s own `.Renviron` affects this
and what to do on a shared or cloud computer.

## Example

Find every kind of service within 5 miles of a ZIP code:

``` r
library(CDCLocator)

providers <- cdc_providers("20176", radius = 5)
```

`cdc_providers()` returns the API’s records with their details nested.
To get one row per provider with plain columns, pass them to
`cdc_providers_tidy()`:

``` r
tidy <- cdc_providers_tidy(providers)
tidy[, c("name", "street", "city", "contact_telephone", "distance")]
```

Search for particular services only. The names are listed in
`?cdc_providers`:

``` r
prep <- cdc_providers("20176", radius = 10, services = c("preptesting", "pep"))
```

Start from a place or a point instead of a ZIP code:

``` r
cdc_address_place("Peoria", "IL")
cdc_address_point(40.69365, -89.58899)
```

Both return a one-row tibble with the ZIP code, which you can pass to
`cdc_providers()`.

## Things to know

- **ZIP codes are strings.** Write `"02110"`, not `02110`, or the
  leading zero is lost.
- **Searching every service takes several requests.** The API returns
  nothing when all 14 services are requested at once, so the package
  asks for three at a time and combines the results. The default search
  makes five requests, spaced half a second apart, so it takes a few
  seconds.
- **The same clinic can appear more than once** when it offers several
  services. See [Working with
  results](https://vagishhemmige.github.io/CDCLocator/articles/working-with-results.html).
- **Some networks are refused.** The API has refused requests from
  outside the US and from some cloud servers. If you get an HTTP 403
  error, see
  [Troubleshooting](https://vagishhemmige.github.io/CDCLocator/articles/troubleshooting.html).

## Learn more

The guides are on the [package
website](https://vagishhemmige.github.io/CDCLocator/). If you installed
with `build_vignettes = TRUE`, they are also available in R with
`vignette()`:

| Guide                                                                                                                                                                                 | In R                                   |
|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------|
| [Getting started](https://vagishhemmige.github.io/CDCLocator/articles/CDCLocator.html): a first search, step by step                                                                  | `vignette("CDCLocator")`               |
| [Credentials and networks](https://vagishhemmige.github.io/CDCLocator/articles/credentials-and-networks.html): getting and storing credentials, and where the API can be reached from | `vignette("credentials-and-networks")` |
| [Services and searching](https://vagishhemmige.github.io/CDCLocator/articles/services-and-searching.html): the 14 services, the radius and how searches are made                      | `vignette("services-and-searching")`   |
| [Working with results](https://vagishhemmige.github.io/CDCLocator/articles/working-with-results.html): the columns, duplicates, mapping and saving                                    | `vignette("working-with-results")`     |
| [Troubleshooting](https://vagishhemmige.github.io/CDCLocator/articles/troubleshooting.html): what each error means and what to do                                                     | `vignette("troubleshooting")`          |
