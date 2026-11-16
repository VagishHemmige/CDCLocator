# cdc_providers() validates its arguments

    Code
      cdc_providers(2110)
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a non-empty character scalar.

---

    Code
      cdc_providers("")
    Condition
      Error in `cdc_providers()`:
      ! `zipcode` must be a non-empty character scalar.

---

    Code
      cdc_providers("02110", radius = "10")
    Condition
      Error in `cdc_providers()`:
      ! `radius` must be a single numeric value.

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
      Error in `.validate_cdc_locator_services()`:
      ! Invalid CDC Locator service enum(s):
      x nope
      i Valid values are:
        hivtesting, stdtesting, selftesting, preptesting, pep, condom, hepc, doxypep, mailintesting, ryanwhite, clinics, substanceabuse, mentalhealth, housing

