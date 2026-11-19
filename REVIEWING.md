# Shared CDCLocator review standards

Adapted from CDCAtlas’s `REVIEWING.md` at
[`fef315a`](https://github.com/VagishHemmige/CDCAtlas/blob/fef315a/REVIEWING.md).

These standards apply to ChatGPT and Claude, within the user’s
permissions. Read
[AGENTS.md](https://vagishhemmige.github.io/CDCLocator/AGENTS.md). A
review is an assessment of a defined change, not automatic permission to
edit, publish, approve or merge.

## Policy revision and coverage

For PRs, fetch AGENTS.md and REVIEWING.md from the actual target branch
at its recorded current base SHA. Record both policy paths and that SHA.
Proposed policy changes do not govern their own review. If retrieval
fails, disclose the gap; do not claim the policy was read. For local
reviews, use committed policy at HEAD and assess uncommitted policy
edits separately.

Identify the copy, branch, exact commit, target, comparison scope and
policy revision. GitHub reviews exclude unpushed changes. Use the merge
base for the initial PR diff. Read changed human-written code and
relevant surrounding paths; trace effects through callers, returned
objects, tests, documentation and affected workflows. Reserve
whole-package coverage claims for whole-package reviews. For a
commissioned whole-package review, follow
[PACKAGE_REVIEW.md](https://vagishhemmige.github.io/CDCLocator/PACKAGE_REVIEW.md).

On follow-up reviews, inspect changes since the last reviewed revision,
keeping relevant full-PR context. Account for changed bases and
rewritten history. Revisit unresolved findings only with new material
evidence; do not repeat resolved findings. For generated-only changes,
check agreement with source, exports and imports and keep the report
proportionate.

## Development-aware judgment

Identify the stated milestone and intended behavior before judging it.
Distinguish: - Verified defects introduced or exposed by the change. -
Validation or evidence gaps, without calling uncertainty a proven
defect. - Design decisions requiring an explicit rationale or user
decision. - Intentional unfinished work within a documented development
milestone. - Unrelated existing limitations and future release-readiness
work.

Prioritize credential exposure, silently wrong or incomplete results,
broken installation or exports, and misleading error messages.
Intentional documented interface changes can be acceptable during
development; accidental breakage remains a defect. Do not demand
unrelated cleanup. When alternatives are reasonable, present their
tradeoffs and a recommendation, following AGENTS.md’s decision process.
Evaluate tests and CI independently: passing assertions can encode an
incorrect assumption about the API. Explain whether external findings
are agreed with, partly agreed with or disagreed with, and why.

## API checks

Verify affected behavior against the current HIV.gov Locator API
documentation where it is available, starting from [HIV.gov’s Locator
API announcement](https://www.hiv.gov/blog/announcing-the-locator-api).
Use an available authorized browser or web-fetch tool; shell network
access is not required. Disclose unavailable sources and the resulting
evidence gap rather than inventing verification.

Check, where affected: - Every request goes through
`.cdc_locator_request()` and `.perform_cdc_locator_request()`, not a
separate httr2 chain. - Input validation: invalid, empty or `NA` ZIP
codes, cities, states, coordinates, radius and service values, and ZIP
codes as character with leading zeros kept. - Responses: no providers
found, empty or missing fields, `NULL` versus `NA`, nested columns that
are absent, and changed response shapes. A function should return a
zero-row tibble with the documented columns, or a clear error, never a
silently malformed result. - Failures: HTTP errors, timeouts and network
failures, with retries and rate limiting where they help. Error messages
should let the user tell apart missing credentials, a rejected token,
bad input and the API being down. - Credentials: never shown in errors,
messages, printed requests, `req_dry_run()` or verbose output, and never
stored in fixtures or snapshots. - Caching, if added: where it writes,
how long entries live, and whether cached results can mix credentials or
users.

HTTP success alone proves none of the above.

## Validation

Choose validation scope for each PR from the changed behavior and its
downstream reach, not the number of changed lines. State the selected
scope and rationale. Run focused checks first; expand when shared
helpers, dependencies or observed failures warrant it. Distinguish
failures introduced by the PR from existing failures. When attributing a
failure to the PR, compare with the target-branch baseline under
equivalent conditions where feasible; otherwise state that attribution
remains uncertain. If adequate validation is unavailable, report the
specific gap.

Relevant CI results from the exact reviewed revision may supply
validation evidence; independently assess whether those tests check the
intended behavior. Keep observed CI distinct from personally executed
checks.

Inspect test fixtures, helpers, startup and load-time effects before
execution. Use only the isolated environment described in AGENTS.md,
with fake credentials. Inspect expectations independently of
implementation; favor small hand-checkable mocked responses and
regression failures before fixes. Mock at the request boundary, keeping
meaningful parsing and tidying checks.

For a fix arising from a review, check that the useful reproducer
becomes ordinary coverage in the existing owning test file, or that the
PR explains why existing coverage suffices. Verify failure before the
fix and success afterwards when feasible.

Keep these evidence categories separate: 1. Source inspection and
reasoning. 2. Personally executed offline checks with mocked responses.
3. Recorded-response checks (with provenance and retrieval date). 4.
Live API checks (request, retrieval date, scope; never credentials). 5.
Observed CI results (revision and job). 6. Author-reported checks, not
independently reproduced.

Never present mocked checks as proof of live compatibility. Report OS, R
and package versions if execution occurred, failures, skipped checks and
material coverage limits. Give exact remaining maintainer commands, with
prerequisites and network effects. Check dependency minimums and
material Windows portability risks (file paths, line endings, encoding,
temporary directories). Broken exports, imports or installation are
substantive defects.

## Reporting

Lead with the change’s purpose and scoped conclusion. Group actionable
findings as Must fix, Should fix or Consider, omitting empty groups. For
each, give source path and line, evidence, failure mode or uncertainty,
practical consequence and suggested resolution. Keep design questions
distinct from verified defects. Avoid stylistic nitpicks and unrelated
backlog items. Explain any pre-existing issue that the change materially
worsens.

State actual checks, policy revision and material limitations. A clean
scoped review means no actionable findings in the inspected scope, not
comprehensive correctness, formal approval or release readiness. Silence
or a skipped or failed automated review is not approval. Self-review is
not independent review.
