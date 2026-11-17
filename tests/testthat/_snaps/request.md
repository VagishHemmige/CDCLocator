# .cdc_locator_request() rejects a query that is not a list

    Code
      .cdc_locator_request("providers", query = "zip")
    Condition
      Error in `.cdc_locator_request()`:
      ! `query` must be a named list.

# API error text is shown, without credentials

    Code
      .perform_cdc_locator_request(.cdc_locator_request("providers"))
    Condition
      Error:
      ! HTTP 500 Internal Server Error.
      i The API said: invalid entry: radius must be 50 or less

# a 403 explains the two usual causes

    Code
      .perform_cdc_locator_request(.cdc_locator_request("providers"))
    Condition
      Error:
      ! HTTP 403 Forbidden.
      i The API said: {"Message":"User is not authorized to access this resource"}
      i The API refused this request. The usual causes are:
      * the client ID or token is wrong or no longer active, or
      * the request came from a network the API blocks: outside the US, or a cloud server or VPN (for example GitHub Actions runners on Microsoft Azure).

