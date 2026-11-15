# cdc_address_point() validates its arguments

    Code
      cdc_address_point("42", -71)
    Condition
      Error in `cdc_address_point()`:
      ! `latitude` must be a single numeric value.

---

    Code
      cdc_address_point(42, c(-71, -72))
    Condition
      Error in `cdc_address_point()`:
      ! `longitude` must be a single numeric value.

