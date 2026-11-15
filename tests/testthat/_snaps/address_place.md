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

