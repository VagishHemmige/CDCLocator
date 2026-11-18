# cdc_address_place() validates its arguments

    Code
      cdc_address_place(1, "MA")
    Condition
      Error in `cdc_address_place()`:
      ! `city` must be a single non-empty string.

---

    Code
      cdc_address_place("Boston", "")
    Condition
      Error in `cdc_address_place()`:
      ! `state` must be a single non-empty string.

# cdc_address_place() rejects missing or blank city and state

    Code
      cdc_address_place(NA_character_, "MA")
    Condition
      Error in `cdc_address_place()`:
      ! `city` must be a single non-empty string.

---

    Code
      cdc_address_place("Boston", NA_character_)
    Condition
      Error in `cdc_address_place()`:
      ! `state` must be a single non-empty string.

---

    Code
      cdc_address_place("   ", "MA")
    Condition
      Error in `cdc_address_place()`:
      ! `city` must be a single non-empty string.

---

    Code
      cdc_address_place(c("Boston", "Quincy"), "MA")
    Condition
      Error in `cdc_address_place()`:
      ! `city` must be a single non-empty string.

