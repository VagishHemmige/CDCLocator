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

