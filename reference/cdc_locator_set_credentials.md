# Set CDC Locator API credentials

Sets the `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN`
environment variables for the current R session, so the other functions
can find them, and, if you ask, saves them in your `.Renviron` file for
later sessions. The values are never printed.

## Usage

``` r
cdc_locator_set_credentials(client_id, client_token, install = FALSE)
```

## Arguments

- client_id:

  Your Locator API client ID, a single string.

- client_token:

  Your Locator API client token, a single string.

- install:

  `FALSE`, the default, sets the credentials for this session only and
  writes no file. `TRUE` also saves them in your `.Renviron` file, after
  asking.

## Value

`NULL`, invisibly. Called for its side effects.

## Details

With `install = TRUE`, the function first asks whether to save the
credentials, naming the file, and writes only after you answer yes. It
writes to the file named by the `R_ENVIRON_USER` environment variable if
that is set, and otherwise to `.Renviron` in the home folder R uses at
startup (on Windows, `R_USER` if set, then `HOME`). Any existing
`CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` lines are
replaced, and every other line is kept. The file is made readable only
by you before the credentials are written to it; if that isn't possible,
nothing is saved. (On Windows, files are protected by the permissions of
your home folder.) Because it must ask, `install = TRUE` only works in
an interactive session.

R reads only one user `.Renviron` when it starts. If the folder you
start R in (in RStudio, the project's folder) has its own `.Renviron`, R
reads that file instead of the one in your home folder, so credentials
saved there don't apply in that project. The function warns you when
this is the case.

Avoid typing the token into a script you might share. Calling this
function with values read from a password manager, or with
[`rstudioapi::askForPassword()`](https://rstudio.github.io/rstudioapi/reference/askForPassword.html),
keeps them out of your code.

## Examples

``` r
if (FALSE) { # \dontrun{
# For this session only
cdc_locator_set_credentials(
  client_id = "your-client-id",
  client_token = "your-client-token"
)

# Also save them for future sessions, after confirming
cdc_locator_set_credentials(
  client_id = "your-client-id",
  client_token = "your-client-token",
  install = TRUE
)
} # }
```
