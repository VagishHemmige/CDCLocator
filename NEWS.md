# CDCLocator (development version)

* Initial development version.
* `cdc_providers()` searches several ZIP codes in one call: give `zipcode` a vector. Each provider is kept once, from the search where it was nearest (`distinct = FALSE` keeps every row); a ZIP code the API doesn't recognize ("No point found for address") is skipped with a warning and listed in `attr(result, "failures")`, and any other error stops the search; requests are spaced one second apart (`min_gap`); and a progress bar shows how far the search has got (`progress`).
* `cdc_providers()` and `cdc_providers_tidy()` results have a new `search_zipcode` column: the ZIP code whose search found each provider.
