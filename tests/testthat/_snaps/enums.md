# .validate_cdc_locator_services() rejects bad input

    Code
      .validate_cdc_locator_services(1)
    Condition
      Error in `.validate_cdc_locator_services()`:
      ! `services` must be a character vector.

---

    Code
      .validate_cdc_locator_services(c("hivtesting", "nope"))
    Condition
      Error in `.validate_cdc_locator_services()`:
      ! Invalid CDC Locator service enum(s):
      x nope
      i Valid values are:
        hivtesting, stdtesting, selftesting, preptesting, pep, condom, Hepc, doxypep, mailintesting, ryanwhite, clinics, substanceabuse, mentalhealth, housing

