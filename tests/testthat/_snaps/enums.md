# .validate_cdc_locator_services() rejects bad input

    Code
      .validate_cdc_locator_services(1)
    Condition
      Error:
      ! `services` must be a character vector.

---

    Code
      .validate_cdc_locator_services(c("hivtesting", "nope"))
    Condition
      Error:
      ! Invalid CDC Locator service enum(s):
      x nope
      i Valid values are:
        hivtesting, stdtesting, selftesting, preptesting, pep, condom, hepc, doxypep, mailintesting, ryanwhite, clinics, substanceabuse, mentalhealth, housing

