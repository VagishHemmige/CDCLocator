# Getting started with CDCLocator

This article walks through a first search: setting up credentials,
finding providers near a ZIP code, turning the results into a table, and
starting from a place instead of a ZIP code. Each step links to a longer
article.

The code here needs a connection to the HIV.gov Locator API. If this
copy of the article was built without one, you see the code but not its
output.

## What the package does

The HIV.gov Locator API lists places that offer HIV testing, prevention
and care, and related services such as hepatitis C testing, mental
health care and housing help. CDCLocator sends your questions to the API
and returns the answers as tibbles.

There are four functions you will use:

| Function | What it does |
|----|----|
| [`cdc_providers()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers.md) | Finds providers within a radius of a ZIP code |
| [`cdc_providers_tidy()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers_tidy.md) | Turns those results into one plain row per provider |
| [`cdc_address_place()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_address_place.md) | Finds the ZIP code for a city and state |
| [`cdc_address_point()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_address_point.md) | Finds the ZIP code for a latitude and longitude |

A fifth,
[`cdc_locator_set_credentials()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_locator_set_credentials.md),
sets your credentials for the session and can save them for later ones.

## Step 1: credentials

The API needs a client ID and a client token. HIV.gov issues them on
request: the [Locator page on HIV.gov](https://www.hiv.gov/locator) says
how to ask, and in November 2026 it pointed to <contact@hiv.gov>.

The best place to keep them is your `.Renviron` file, which R reads when
it starts. The package can save them there for you, after asking:

``` r

cdc_locator_set_credentials(
  client_id = "your-client-id",
  client_token = "your-client-token",
  install = TRUE
)
```

Or open the file with `usethis::edit_r_environ()`, add these two lines
with your own values, save, and restart R:

    CDC_LOCATOR_CLIENT_ID=your-client-id
    CDC_LOCATOR_CLIENT_TOKEN=your-client-token

Check that R can see them. This prints `TRUE TRUE` without showing the
values:

``` r

nzchar(Sys.getenv(c("CDC_LOCATOR_CLIENT_ID", "CDC_LOCATOR_CLIENT_TOKEN")))
```

[`vignette("credentials-and-networks")`](https://vagishhemmige.github.io/CDCLocator/articles/credentials-and-networks.md)
covers other ways to supply them, how to keep the token safe, and which
networks the API accepts.

## Step 2: find providers near a ZIP code

``` r

library(CDCLocator)

providers <- cdc_providers("20176", radius = 5)
providers
```

Three things to notice:

- **The ZIP code is a string.** `"02110"` keeps its leading zero; the
  number `02110` would become `2110`, so the package refuses numbers.
- **`radius` is in miles**, a whole number from 1 to 50. The default is
  10.
- **With no `services` argument, every service is searched.** The API
  returns nothing when all 14 services are asked for at once, so the
  package asks for three at a time: five requests, half a second apart.
  The search takes a few seconds.

The result has one row per provider record and four columns: `type`,
`id`, `search_zipcode` (the ZIP code searched), and `attributes`, which
holds the details in nested form. `zipcode` can also be several ZIP
codes;
[`vignette("services-and-searching")`](https://vagishhemmige.github.io/CDCLocator/articles/services-and-searching.md)
explains how those searches combine.

## Step 3: one row per provider

[`cdc_providers_tidy()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers_tidy.md)
unpacks the details into ordinary columns:

``` r

tidy <- cdc_providers_tidy(providers)
names(tidy)
tidy[, c("name", "street", "city", "zipcode", "contact_telephone", "distance")]
```

`distance` is in miles from the ZIP code you searched. `languages` is a
list column, because a provider can list several.
[`vignette("working-with-results")`](https://vagishhemmige.github.io/CDCLocator/articles/working-with-results.md)
describes every column and shows how to sort, map and save the table.

## Step 4: choose services

To search for particular services, name them:

``` r

testing <- cdc_providers(
  "20176",
  radius = 10,
  services = c("hivtesting", "stdtesting", "hepc")
)
```

A provider is returned if it offers **any** of the services you name.
The 14 service names, and what each covers, are in
[`vignette("services-and-searching")`](https://vagishhemmige.github.io/CDCLocator/articles/services-and-searching.md)
and
[`?cdc_providers`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers.md).
Case doesn’t matter: `"HIVtesting"` works too.

## Step 5: start from a place or a point

The provider search only takes a ZIP code. If you have a city and state,
or a latitude and longitude, look up the ZIP code first:

``` r

peoria <- cdc_address_place("Peoria", "IL")
peoria

cdc_address_point(40.69365, -89.58899)
```

Each returns one row with the city, the full state name, the ZIP code
and its coordinates. Pass the ZIP code on:

``` r

cdc_providers(peoria$zipcode, radius = 10, services = "hivtesting") |>
  cdc_providers_tidy()
```

## When something goes wrong

Errors say which function and argument they’re about, and pass on what
the API said. The most common ones:

- **“CDC Locator API credentials not found”**: R can’t see your
  credentials. Go back to step 1.
- **HTTP 403 Forbidden**: either the key was refused, or your network
  was. The API has refused requests from outside the US and from some
  cloud servers.
- **HTTP 500 with “No point found for address”**: the API doesn’t know
  that ZIP code.

[`vignette("troubleshooting")`](https://vagishhemmige.github.io/CDCLocator/articles/troubleshooting.md)
lists every error with what to do about it.
