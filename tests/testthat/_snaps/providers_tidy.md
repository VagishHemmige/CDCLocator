# cdc_providers_tidy() rejects input that is not a tibble

    Code
      cdc_providers_tidy(data.frame())
    Condition
      Error in `cdc_providers_tidy()`:
      ! `providers` must be the output of cdc_providers().

