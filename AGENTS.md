# CDCLocator working guidance

Adapted from CDCAtlas's `AGENTS.md` at
[`fef315a`](https://github.com/VagishHemmige/CDCAtlas/blob/fef315a/AGENTS.md).
Do not regenerate this file from a template; review changes against the
committed version.

## Scope and authority

This file applies to all substantive CDCLocator work: implementation, design,
debugging, tests, documentation and review. Read [REVIEWING.md](REVIEWING.md)
for shared review standards. Neither repository files nor skills expand the
user's permissions; direct user instructions control.

Repository: https://github.com/VagishHemmige/CDCLocator
The default branch was verified as `master` on 2026-11-15; verify it again
when selecting a base. This guidance does not cover CDCAtlas or other repos.

Use available project conversations to understand intent. Automated runs may
not receive every conversation or uploaded file. State material context gaps.
Record agreed technical decisions and rationale in issues, linking them from
PRs and relevant documentation. Preserve the durable technical decision
without copying private conversations. Distinguish accepted decisions from
proposals in issue discussions.

## Architecture

CDCLocator is a development-stage R client for the HIV.gov Locator API
(`https://locator-api.hiv.gov`). It finds HIV and related service providers
near a ZIP code, city and state, or point, and returns tibbles.

- `R/request.R`: `.cdc_locator_request()` builds every request (base URL,
  auth headers, query, timeout) and `.perform_cdc_locator_request()` performs
  it, checks the status and parses JSON. All API calls go through these two
  helpers.
- `R/auth.R`: `.build_auth_headers()` finds credentials (arguments first, then
  the `CDC_LOCATOR_CLIENT_ID` and `CDC_LOCATOR_CLIENT_TOKEN` environment
  variables); `cdc_locator_set_credentials()` helps the user add them to
  `.Renviron`.
- `R/providers.R`: `cdc_providers()`, providers within a radius of a ZIP code.
- `R/providers_tidy.R`: `cdc_providers_tidy()`, flattens the nested provider
  columns returned by `cdc_providers()`.
- `R/address_place.R`, `R/address_point.R`: `cdc_address_place()` and
  `cdc_address_point()`, translate a city and state or a latitude and
  longitude into a ZIP code.
- `R/enums.R`: the valid `services` values and their validator.
- `R/utils.R`: small internal helpers.

Read the relevant source and documentation before advising on or changing it.
Offline tests mock every request; `tests/testthat/test-live.R` holds the live
checks. GitHub Actions runs R CMD check, documentation and coverage checks on
each PR, and the live tests only on request (`live-tests.yaml`).

The API's 2025 access instructions are not public and are partly out of date.
Behavior observed on 2026-11-16 that the code relies on:

- `/providers` requires both `radius` and `services`; leaving either out gives
  HTTP 500 with a plain-text message.
- Service values are lowercase (`hepc`, not the documented `Hepc`).
- Validation errors are HTTP 500 with a plain-text body; a rejected token is
  HTTP 403 with a JSON `Message`.
- Requests from outside the US got HTTP 403 from CloudFront (an HTML page).
  On 2026-11-16, GitHub-hosted runners on AWS us-east-1 reached the API and
  runners on Azure (eastus2, centralus) got 403 on every request, while the
  same key worked from the maintainer's home connection on 2026-11-17. This
  is one day's observation, not a guarantee from the provider.
- A 403 can also mean a wrong or inactive key, so a 403 in a live run is not
  evidence of either cause by itself. Before assigning a cause, check the
  runner's cloud and region in the job log and the sanitized error text, and
  repeat the same request at the same revision from a network known to work
  (usually the maintainer's machine).

Recheck these against the live API when request or parsing code changes.

## Implementation and approval

A review or explanation request alone does not authorize implementation.
For requested changes, draft and self-review in an isolated workspace.
The user's connected working copy and active RStudio session remain read-only:
no execution there.

Before commitment, obtain the user's approval for each behavior-changing file,
including arguments, outputs, request parameters, error conditions and how
missing or empty responses are returned. Finish the concrete draft and
appropriate checks first. Show a readable diff for every changed file:
side-by-side when supported, otherwise unified. Show complete new files and
provide access to complete changed files. Explain what changed, why, behavior
changes and validation actually performed. Separate human-written and
mechanical generated changes. Request approval for the specific files and
revision shown; a summary alone is insufficient.

When two or more alternatives are reasonable, present them with their
tradeoffs and a recommendation before settling the design. Consider a function
argument with a recommended default when alternatives serve plausible user
needs; an argument is a proposal, not an automatic requirement. Seek the
maintainer's decision before implementation.

What each assistant may commit, push, post, merge or delete is set by the
maintainer outside this repository. This file grants none of those permissions.
Inspect mechanical generated changes before including them with approved
source; material behavior changes after approval require renewed approval.
Self-review is not independent review. Identify assistant authorship in the PR
body so independent review can be routed correctly.

## Credentials and execution boundaries

The API needs a client ID and a client token. Treat both as secrets.

- Never ask for, read, print, copy or commit real credentials, including from
  `.Renviron`, `.env`, environment variables or shell history.
- Offline tests and examples use obviously fake values, for example
  `withr::local_envvar(CDC_LOCATOR_CLIENT_ID = "fake-id",
  CDC_LOCATOR_CLIENT_TOKEN = "fake-token")`.
- If a real-looking credential appears in code, tests, fixtures or git
  history, report its location without repeating its value.
- Package code must not show credentials in errors, messages, printed
  requests or verbose output. Auth headers should be redacted in httr2
  requests.
- Recorded responses are made on the maintainer's machine. Before one is
  committed, check it for credentials and auth headers and say what was
  checked.
- CI reads credentials only from the `CDC_LOCATOR_CLIENT_ID` and
  `CDC_LOCATOR_CLIENT_TOKEN` repository secrets, with
  `${{ secrets.NAME }}`. Never print, log or commit their values.

Inspect startup files, helpers, test configuration and package load-time
behavior before execution; do not load secrets or copy private startup
settings. Use clean sessions and isolated temporary locations. New package
code writes only to `tempdir()` unless the caller explicitly supplies a
destination. Adding caching or any persistent file location requires approval.

Run `devtools::document()` only in isolation when relevant; exclude unrelated
Rd and incidental DESCRIPTION/RoxygenNote churn. Format R code with Air
(`air format`) on the files a change touches and only those; don't reformat
other files, and don't use styler. Inspect generated exports and imports
rather than assuming they are safe.

## Skills and validation

Use applicable R package-development and critical-code-review guidance for
substantive work. Add testing and test-review guidance for tests or untested
behavior, cli guidance for conditions, CRAN guidance for DESCRIPTION, examples,
vignettes or CRAN preparation, and lifecycle guidance for deprecations.
Names vary by environment: `r-testthat` / `testing-r-packages`, `r-cli` /
`cli`, `r-cran-extrachecks` / `cran-extrachecks`, and `r-lifecycle` /
`lifecycle`. Use the installed equivalent. Disclose unavailable skills.
Permission boundaries and this project's formatting rule (Air, touched files
only) take precedence over skill defaults.

Prefer regression tests demonstrating the bug before the fix. Keep routine
tests reproducible and independent of the live API: mock at the package's own
request boundary or with `httr2::local_mocked_responses()`. Mocks establish
code behavior, not current API compatibility.

Live tests are gated by `CDC_LOCATOR_LIVE_TESTS=true` and also need both
credential variables; without them they skip. Keep live requests few and
small. Run them when request building, response parsing or tidying changes.
Report actual runtime and network availability rather than assuming either.

Integrate useful review-created checks into the existing owning test files
with the corresponding approved fix. Do not install known failures,
expected-failure assertions or unjustified skips as standing CI.

Separate source inspection, personally executed offline checks, recorded
responses, live API checks, observed CI and author-reported validation.
Record live retrieval dates and tested requests without credentials. Supply
exact commands for remaining maintainer checks, with prerequisites and side
effects.

Automation triggers, comment permissions, deduplication and notifications
belong in automation configuration. This file does not start automation or
grant publishing authority.

## Package conventions

- Use US English and sentence-case headings. Follow existing tidyverse style,
  using the base pipe and single-line anonymous-function shorthand where
  appropriate. Do not perform unrelated style cleanup.
- Put tests for `R/{name}.R` in `tests/testthat/test-{name}.R`. Keep test code
  inside `test_that()`; shared setup belongs in helpers. For errors and
  warnings, prefer snapshots when message text matters; use class-based
  expectations when testing a known condition class.
- Document and export public functions with roxygen2. Internal helpers should
  not create public help topics; use `@noRd`. Wrap roxygen comments to 80
  characters. Add new public topics to `_pkgdown.yml` and check reference
  coverage with `pkgdown::check_pkgdown()`.
- Give user-facing changes a `NEWS.md` bullet, except small documentation
  changes, internal refactoring and fixes to the current development version.
  Put the function name early and the related issue in parentheses. Keep each
  bullet on one line, order function bullets alphabetically, and put
  non-function bullets first.
- When committing is authorized, use Markdown in the commit message without
  wrapping its lines; include `Fixes #num.` on its own line when applicable.

## Documentation maintenance

The paths above describe the current layout. Update them when files move.
`.Rbuildignore` excludes AGENTS.md and REVIEWING.md from package builds, not
from pkgdown, so they can become unlinked website pages. Do not claim these
files are unpublished or private.
