# cdc_providers() validates its arguments

    Code
      cdc_providers(2110)
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a string, such as "02110".
      i Numbers drop leading zeros: 02110 becomes 2110.

---

    Code
      cdc_providers("")
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a single non-empty string.

---

    Code
      cdc_providers("02110", radius = "10")
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be a single number, not missing or infinite.

---

    Code
      cdc_providers("02110", radius = 51)
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be between 1 and 50 miles.

---

    Code
      cdc_providers("02110", services = "nope")
    Condition
      Error in `cdc_providers()`:
      ! Invalid CDC Locator service enum(s):
      x nope
      i Valid values are:
        hivtesting, stdtesting, selftesting, preptesting, pep, condom, hepc, doxypep, mailintesting, ryanwhite, clinics, substanceabuse, mentalhealth, housing

# cdc_providers() rejects an empty services vector without a request

    Code
      cdc_providers("02110", services = character())
    Condition
      Error in `cdc_providers()`:
      ! `services` must name at least one service.
      i Use `services = NULL` to search every service.

# cdc_providers() treats only an empty array as no matches

    Code
      cdc_providers("20176")
    Condition
      Error in `cdc_providers()`:
      ! The CDC Locator API returned an unexpected response.
      i Expected a JSON array of providers.

---

    Code
      cdc_providers("20176")
    Condition
      Error in `cdc_providers()`:
      ! The CDC Locator API returned an unexpected response.
      i Expected a JSON array of providers.

---

    Code
      cdc_providers("20176")
    Condition
      Error in `cdc_providers()`:
      ! The CDC Locator API returned an unexpected response.
      i Expected a JSON array of providers.

# cdc_providers() rejects missing, short or long ZIP codes before any request

    Code
      cdc_providers(NA_character_)
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a single non-empty string.

---

    Code
      cdc_providers("2110")
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a five-digit ZIP code, such as "02110".
      x You supplied "2110".

---

    Code
      cdc_providers("021101")
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a five-digit ZIP code, such as "02110".
      x You supplied "021101".

---

    Code
      cdc_providers("02110-1234")
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a five-digit ZIP code, such as "02110".
      x You supplied "02110-1234".

# cdc_providers() explains that a numeric ZIP code loses leading zeros

    Code
      cdc_providers(2110)
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a string, such as "02110".
      i Numbers drop leading zeros: 02110 becomes 2110.

# cdc_providers() rejects a missing or fractional radius

    Code
      cdc_providers("02110", radius = NA_real_)
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be a single number, not missing or infinite.

---

    Code
      cdc_providers("02110", radius = 10.5)
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be a whole number.

---

    Code
      cdc_providers("02110", radius = Inf)
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be a single number, not missing or infinite.

---

    Code
      cdc_providers("02110", radius = 0)
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be between 1 and 50 miles.

# cdc_providers() checks every ZIP code before any request

    Code
      cdc_providers(c("20176", "2110", "abcde"))
    Condition
      Error in `cdc_providers()`:
      ! Each `zipcode` must be a five-digit ZIP code, such as "02110".
      x Not valid: "2110" and "abcde".

---

    Code
      cdc_providers(c("20176", NA))
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` can't contain missing or empty values.

---

    Code
      cdc_providers(c("20176", ""))
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` can't contain missing or empty values.

---

    Code
      cdc_providers(character())
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a character vector of ZIP codes, such as "02110".

---

    Code
      cdc_providers(c(20176, 2110))
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a string, such as "02110".
      i Numbers drop leading zeros: 02110 becomes 2110.

# cdc_providers() skips a failing ZIP code among several

    Code
      out <- cdc_providers(c("20176", "00000", "10001"), services = "hivtesting",
      progress = FALSE)
    Condition
      Warning:
      Skipped 1 ZIP code whose search failed: "00000".
      i See `attr(result, "failures")` for the API's messages.

# cdc_providers() rejects a min_gap below the package's minimum

    Code
      cdc_providers("20176", min_gap = 0.1)
    Condition
      Error in `cdc_providers()`:
      ! `min_gap` must be between 0.5 and 60 seconds.

---

    Code
      cdc_providers("20176", min_gap = "1")
    Condition
      Error in `cdc_providers()`:
      ! `min_gap` must be a single number, not missing or infinite.

# cdc_providers() checks progress and distinct

    Code
      cdc_providers("20176", progress = NA)
    Condition
      Error in `cdc_providers()`:
      ! `progress` must be `TRUE` or `FALSE`.

---

    Code
      cdc_providers("20176", distinct = "yes")
    Condition
      Error in `cdc_providers()`:
      ! `distinct` must be `TRUE` or `FALSE`.

# cdc_providers() summarizes a search of several ZIP codes

    Code
      out <- cdc_providers(c("20176", "20147"), services = "hivtesting")
    Message
      v Searched 2 ZIP codes: 2 providers.

---

    Code
      out <- cdc_providers(c("20176", "00000"), services = "hivtesting")
    Message
      v Searched 2 ZIP codes: 1 provider; 1 ZIP code failed.
    Condition
      Warning:
      Skipped 1 ZIP code whose search failed: "00000".
      i See `attr(result, "failures")` for the API's messages.

