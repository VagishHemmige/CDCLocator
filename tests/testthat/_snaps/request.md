# .cdc_locator_request() rejects a query that is not a list

    Code
      .cdc_locator_request("providers", query = "zip")
    Condition
      Error in `.cdc_locator_request()`:
      ! `query` must be a named list.

