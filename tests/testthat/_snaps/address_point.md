# cdc_address_point() validates its arguments

    Code
      cdc_address_point("42", -71)
    Condition
      Error in `cdc_address_point()`:
      ! `latitude` must be a single number, not missing or infinite.

---

    Code
      cdc_address_point(42, c(-71, -72))
    Condition
      Error in `cdc_address_point()`:
      ! `longitude` must be a single number, not missing or infinite.

# cdc_address_point() rejects missing or out-of-range coordinates

    Code
      cdc_address_point(NA_real_, -71)
    Condition
      Error in `cdc_address_point()`:
      ! `latitude` must be a single number, not missing or infinite.

---

    Code
      cdc_address_point(42, NA_real_)
    Condition
      Error in `cdc_address_point()`:
      ! `longitude` must be a single number, not missing or infinite.

---

    Code
      cdc_address_point(91, -71)
    Condition
      Error in `cdc_address_point()`:
      ! `latitude` must be between -90 and 90.

---

    Code
      cdc_address_point(42, -181)
    Condition
      Error in `cdc_address_point()`:
      ! `longitude` must be between -180 and 180.

---

    Code
      cdc_address_point(Inf, -71)
    Condition
      Error in `cdc_address_point()`:
      ! `latitude` must be a single number, not missing or infinite.

