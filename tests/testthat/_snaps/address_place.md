# cdc_address_place() validates its arguments

    Code
      cdc_address_place(1, "MA")
    Condition
      Error in `cdc_address_place()`:
      ! `city` must be a non-empty character scalar.

---

    Code
      cdc_address_place("Boston", "")
    Condition
      Error in `cdc_address_place()`:
      ! `state` must be a non-empty character scalar.

# cdc_address_place() errors name the function, not httr2

    Code
      cdc_address_place("Boston", "MA")
    Condition
      Error in `cdc_address_place()`:
      ! HTTP 500 Internal Server Error.
      i The API said: city is not provided

