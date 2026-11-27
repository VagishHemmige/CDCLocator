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
      i To keep them for future sessions, run this again with `install = TRUE`, or add `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` to your '.Renviron' yourself and restart R.

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

# install = TRUE needs an interactive session and changes nothing without one

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `install = TRUE` needs an interactive session, so you can confirm it.
      i Nothing was set or saved.

# install = TRUE asks first and writes nothing if the answer is no

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Message
      v Locator API credentials set for this R session.
      i Not saved to '<home>/.Renviron'.

# install = TRUE refuses a .Renviron that is a symbolic link

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! '<home>/.Renviron' is a symbolic link, so it can't be updated safely.
      i Use `install = FALSE`, or add the credentials to the file it points to yourself.
      i Nothing was set or saved.

# install = TRUE saves nothing if it can't tell whether the file is a link

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't check whether '<home>/.Renviron' is a link, so it wasn't updated.
      i Use `install = FALSE`, or add the credentials to it yourself.
      i Nothing was set or saved.

# install = TRUE refuses values .Renviron can't hold, before writing

    Code
      cdc_locator_set_credentials("fake id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_id` has characters that can't be saved safely in '.Renviron'.
      i Only letters, digits and `. _ ~ + / = : -` can be saved this way.
      i Use `install = FALSE` to set it for this session only.
      i Nothing was set or saved.

---

    Code
      cdc_locator_set_credentials("fake-client-id", "fake\ntoken", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_token` has characters that can't be saved safely in '.Renviron'.
      i Only letters, digits and `. _ ~ + / = : -` can be saved this way.
      i Use `install = FALSE` to set it for this session only.
      i Nothing was set or saved.

# after a refusal, install = FALSE doesn't suggest saving again

    Code
      cdc_locator_set_credentials("fake-client-id", "fake#token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `client_token` has characters that can't be saved safely in '.Renviron'.
      i Only letters, digits and `. _ ~ + / = : -` can be saved this way.
      i Use `install = FALSE` to set it for this session only.
      i Nothing was set or saved.

---

    Code
      cdc_locator_set_credentials("fake-client-id", "fake#token")
    Message
      v Locator API credentials set for this R session.
      i These values can't be saved in '.Renviron', so set them again in each new session.

# install must be TRUE or FALSE

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = "yes")
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! `install` must be `TRUE` or `FALSE`.

# install = TRUE restores the umask when the file can't be created

    Code
      withr::with_options(list(warn = 2), cdc_locator_set_credentials(
        "fake-client-id", "fake-client-token", install = TRUE))
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't make a file in '<home>/blocker' readable only by you.
      i The credentials are set for this session, but nothing was saved.
      i Add them to '<home>/blocker/.Renviron' yourself, and make it readable only by you.

# install = TRUE keeps the old file if writing the new one fails

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't write a new copy of '<home>/.Renviron'.
      i The credentials are set for this session, but nothing was saved.
      i Add them to '<home>/.Renviron' yourself, and make it readable only by you.

# install = TRUE keeps the old file if closing the new one warns

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't write a new copy of '<home>/.Renviron'.
      i The credentials are set for this session, but nothing was saved.
      i Add them to '<home>/.Renviron' yourself, and make it readable only by you.

# install = TRUE never removes a file that already had the temporary name

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't make a file in '<home>' readable only by you.
      i The credentials are set for this session, but nothing was saved.
      i Add them to '<home>/.Renviron' yourself, and make it readable only by you.

# install = TRUE keeps the old file if replacing it fails

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't replace '<home>/.Renviron'.
      i The credentials are set for this session, but nothing was saved.
      i Add them to '<home>/.Renviron' yourself, and make it readable only by you.

# install = TRUE saves nothing if the file can't be protected

    Code
      cdc_locator_set_credentials("fake-client-id", "fake-client-token", install = TRUE)
    Condition
      Error in `cdc_locator_set_credentials()`:
      ! Couldn't make a file in '<home>' readable only by you.
      i The credentials are set for this session, but nothing was saved.
      i Add them to '<home>/.Renviron' yourself, and make it readable only by you.

