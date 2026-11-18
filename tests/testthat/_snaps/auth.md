# .build_auth_headers() errors helpfully without credentials

    Code
      .build_auth_headers()
    Condition
      Error in `.build_auth_headers()`:
      ! CDC Locator API credentials not found.
      i Set credentials using `cdc_locator_set_credentials()`
      i or define environment variables:
        `CDC_LOCATOR_CLIENT_ID`
        `CDC_LOCATOR_CLIENT_TOKEN`

# cdc_locator_set_credentials() explains how to keep them

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token")
    Message
      v Locator API credentials set for this R session.
      i To keep them, add `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` to your '.Renviron' (for example with `usethis::edit_r_environ()`), then restart R.

# cdc_locator_set_credentials() rejects missing or malformed values

    Code
      cdc_locator_set_credentials("", "fake-client-token")
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_id` must be a single non-empty string.

---

    Code
      cdc_locator_set_credentials("fake-client-id", NA)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_token` must be a single non-empty string.

---

    Code
      cdc_locator_set_credentials(c("a", "b"), "fake-client-token")
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_id` must be a single non-empty string.

---

    Code
      cdc_locator_set_credentials("fake-client-id")
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_token` is missing.

