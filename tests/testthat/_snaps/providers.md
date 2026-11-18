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

---

    Code
      cdc_providers(c("02110", "20176"))
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a single non-empty string.

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

