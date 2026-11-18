# Set CDC Locator API credentials for this session

Sets the `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN`
environment variables for the current R session, so the other functions
can find them. The values are never printed, and no file is written.

## Usage

``` r
cdc_locator_set_credentials(client_id, client_token)
```

## Arguments

- client_id:

  Your Locator API client ID, a single string.

- client_token:

  Your Locator API client token, a single string.

## Value

`NULL`, invisibly. Called for its side effect of setting the two
environment variables.

## Details

To keep the credentials between sessions, add both variables to your
`.Renviron` file instead, for example with
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html),
and restart R:

    CDC_LOCATOR_CLIENT_ID=your-client-id
    CDC_LOCATOR_CLIENT_TOKEN=your-client-token

Avoid typing the token into a script you might share. Calling this
function with values read from a password manager, or with
[`rstudioapi::askForPassword()`](https://rstudio.github.io/rstudioapi/reference/askForPassword.html),
keeps them out of your code.

## Examples

``` r
if (FALSE) { # \dontrun{
cdc_locator_set_credentials(
  client_id = "your-client-id",
  client_token = "your-client-token"
)
} # }
```
