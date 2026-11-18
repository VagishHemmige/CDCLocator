# Credentials and networks

The Locator API only answers requests that carry a client ID and a
client token, and only from some networks. This article covers how to
get the credentials, where to keep them, how to keep the token safe, and
what to do when the network is the problem.

## Getting credentials

HIV.gov issues a client ID and a client token on request. The [Locator
page on HIV.gov](https://www.hiv.gov/locator) says how to ask; in
November 2026 it pointed to <contact@hiv.gov>. Keep the email they send:
it is your only record of the token.

The ID identifies you, and the token works like a password. The package
sends both with every request, in HTTP headers.

## Where the package looks

Each function that calls the API
([`cdc_providers()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_providers.md),
[`cdc_address_place()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_address_place.md)
and
[`cdc_address_point()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_address_point.md))
finds credentials in this order:

1.  The `client_id` and `client_token` arguments, if you pass them.
2.  The environment variables `CDC_LOCATOR_CLIENT_ID` and
    `CDC_LOCATOR_CLIENT_TOKEN`.

If neither is set, it stops with “CDC Locator API credentials not found”
and sends nothing.

## Option 1: your `.Renviron` file (recommended)

R reads `.Renviron` each time it starts, so credentials stored there are
always available and never appear in your code. Open the file with:

``` r

usethis::edit_r_environ()
```

Add two lines, with your own values and no quotes:

    CDC_LOCATOR_CLIENT_ID=your-client-id
    CDC_LOCATOR_CLIENT_TOKEN=your-client-token

Save the file, make sure it ends with a blank line, and restart R. Then
check, without printing the values:

``` r

nzchar(Sys.getenv(c("CDC_LOCATOR_CLIENT_ID", "CDC_LOCATOR_CLIENT_TOKEN")))
```

Both should be `TRUE`.

### Which `.Renviron` R reads

R reads **one** user `.Renviron` file when it starts, not several:

1.  the file named by the `R_ENVIRON_USER` environment variable, if it’s
    set;
2.  otherwise, a `.Renviron` in the folder R starts in, which in RStudio
    is the project’s folder;
3.  otherwise, the `.Renviron` in your home folder.

So if a project has its own `.Renviron`, R uses that file **instead of**
the one in your home folder, and credentials stored only in the home
file are missing in that project. Either add the two lines to the
project’s file too, or keep them only in the home file and don’t give
the project its own.

[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
opens the home file, and `usethis::edit_r_environ(scope = "project")`
opens the project’s. If you use a project file, add `.Renviron` to the
project’s `.gitignore` so it is never committed. To work out which file
R would have read, check:

``` r

Sys.getenv("R_ENVIRON_USER")   # if set, R read this file
file.exists(".Renviron")       # is there a .Renviron in the current folder?
```

If `R_ENVIRON_USER` is empty and the current folder is still the one R
started in (in RStudio, the project’s folder), a `TRUE` from
[`file.exists()`](https://rdrr.io/r/base/files.html) means R read that
local file rather than the one in your home folder.

## Option 2: for one session

[`cdc_locator_set_credentials()`](https://vagishhemmige.github.io/CDCLocator/reference/cdc_locator_set_credentials.md)
sets both environment variables until R restarts. It never prints them
and writes no file:

``` r

cdc_locator_set_credentials(
  client_id = "your-client-id",
  client_token = "your-client-token"
)
```

Typing the token into a script puts it wherever the script goes. To
avoid that, read it at run time instead, for example with
[`rstudioapi::askForPassword()`](https://rstudio.github.io/rstudioapi/reference/askForPassword.html)
or the keyring package:

``` r

cdc_locator_set_credentials(
  client_id = rstudioapi::askForPassword("Locator client ID"),
  client_token = rstudioapi::askForPassword("Locator client token")
)
```

## Option 3: pass them to each call

Every function that calls the API takes `client_id` and `client_token`
arguments. These override the environment variables for that call only.
This is mainly useful when one script uses two sets of credentials.

## Keeping the token safe

- **Keep it out of code.** Don’t type it into scripts, R Markdown files
  or Quarto documents you might share.
- **Keep it out of Git.** Never commit `.Renviron`. If a token is
  committed by mistake, removing it in a later commit doesn’t remove it
  from the history: ask HIV.gov for a new one.
- **Use secret storage for automation.** On GitHub Actions, store the
  two values as repository secrets and pass them to the job as
  environment variables.

The package helps too: it marks both headers as secret, so printing a
request or turning on verbose output shows `<REDACTED>`, and it removes
both values from any error text the API sends back before showing it.

## Which networks the API accepts

In November 2026, the API refused some networks with **HTTP 403
Forbidden**, even with valid credentials:

- **Outside the US**: requests from a Canadian server got a “blocked
  from your country” page.
- **Some cloud servers in the US**: GitHub Actions runners hosted on
  Microsoft Azure were refused, while runners on Amazon Web Services and
  a home internet connection worked with the same credentials.

These were observations on particular days, not rules HIV.gov has
published, and they may change. In practice:

- **A home or office connection in the US** has worked.
- **A VPN** may route you through a server the API refuses. If you get a
  403 on a VPN, try without it.
- **A cloud service** (Posit Cloud, a hosted RStudio Server, Google
  Colab, a cloud virtual machine, GitHub Actions) may be refused.
  Whether it is depends on which provider and region it runs on.

## Telling a network refusal from a key problem

Both give HTTP 403, and the package’s error lists both causes. To tell
them apart:

1.  **Read the API’s message in the error.** A refused key has come back
    with a short message such as “User is not authorized to access this
    resource”. The country block came back as a web page, which the
    package doesn’t show, so that error has no “The API said” line. What
    the cloud-server refusals said wasn’t recorded, so a message alone
    may not settle it.
2.  **Try a different network.** If the same credentials work from a
    home connection, the network was the problem.
3.  **Check the credentials themselves**: no extra spaces or quotes in
    `.Renviron`, and R restarted after editing it.

If the credentials fail from a network that has worked before, contact
HIV.gov: the token may have expired or been revoked.
