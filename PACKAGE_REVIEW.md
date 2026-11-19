# CDCLocator whole-package review

Use this protocol when the maintainer commissions a whole-package review. Read [AGENTS.md](AGENTS.md) and [REVIEWING.md](REVIEWING.md) at the reviewed revision. The latter governs individual PRs and defines evidence categories and reporting standards. The purpose here is to establish whether researchers can construct reproducible searches, identify the unit of observation, and interpret service distribution and geographic access within the limits of the directory.

## 1. Establish scope and independent perspectives

Record the copy, branch, full SHA, package version, date, policy revision and scope. Include local changes if reviewing a connected copy. A GitHub review excludes unpushed changes. Record the public site URL and, where discoverable, the source SHA of its successful deployment. A Pages branch commit can differ from its source SHA.

Inventory every tracked file, exported function, internal helper, test, workflow, documentation topic, article and public-facing page or figure. Include every open issue and PR, paginating if necessary. Assign coverage in one working table with item, revision, reviewer, evidence, result and gap. Inspect human-written sources, compare generated outputs with their inputs, and account for exclusions. Keep proposed changes distinct from the reviewed baseline.

| Perspective | Assignment |
| --- | --- |
| R package engineer and API reviewer | Trace requests, parsing, public contracts, dependencies, compatibility and credential handling. |
| Independent test reviewer | Establish expected results independently, challenge mocks, exercise complete workflows and inspect CI. |
| HIV health-services researcher | Assess service definitions, directory ascertainment and what counts of listed services can support about the distribution of HIV services. |
| Spatial epidemiologist | Assess search origins, radius and distance, overlapping catchments, geographic units, denominators and the interpretation of access measures. |
| Documentation and accessibility reviewer | Inspect the published site, navigation, examples, figures, captions and text alternatives. |
| Research analyst new to CDCLocator | Attempt the tasks in section 4 with ordinary R knowledge and only the published materials an analyst could find. |
| Source reviewer | Verify endpoint, service and interpretation claims against the applicable public documentation and identify the limits of the evidence. |

Use independent fresh contexts where supported. Give reviewers the frozen source or public site appropriate to their role and neutral task briefs. Withhold prior diagnoses and other reviewers’ conclusions until reconciliation. If one person or assistant covers several roles, disclose that limitation. Identify simulations as simulations. Keep private conversations and credentials out of reviewer packets.

Apply the installed Posit guidance for R package development and critical code review. Use testthat and testing review for coverage, alt-text guidance for figures, cli guidance for cli output, lifecycle guidance for renames or deprecations, and CRAN extra checks for release, DESCRIPTION, examples or vignettes. Record applicable guidance used or unavailable and what the reviewer did to compensate.

## 2. Trace the API and the meaning of a result

Start from a public entry point and follow its entire path. At the revision used to draft this guide the exports are cdc_locator_set_credentials(), cdc_address_place(), cdc_address_point(), cdc_providers() and cdc_providers_tidy(). Refresh the list from NAMESPACE. For each, compare stated purpose, inputs, output shape, side effects and failures with the implementation, help, tests and downstream examples.

| Path | Questions to resolve with evidence |
| --- | --- |
| Credentials | Where are credentials read, set and optionally persisted? Exercise cancellation, existing files and errors with dummy values. Before calling `install = TRUE`, set `R_ENVIRON_USER` to the absolute path of a disposable `.Renviron` and verify that `.renviron_path()` resolves to that path. Inspect whether values appear in output, requests, logs or snapshots and whether unrelated file contents and permissions survive. |
| Location and request construction | Trace city/state and point lookups through ZIP-based search. Check character ZIPs with leading zeros, coordinate order and ranges, missing and invalid inputs, service identifiers, endpoints, radius, units and actual search origin. Identify defaults and who supplies them. |
| Multiple requests | Check service normalization, batch boundaries, rate limits, retries, timeouts, pagination or result caps where documented. What happens when one batch fails after another succeeds? Check cache keys and invalidation if a cache exists. |
| Provider identity | Establish what one raw row, tidy row and reported count represent. Exercise repeated IDs across batches, the same ID under different services, and different IDs for the same physical site. Trace any deduplication, joins and multi-ZIP examples, including what happens to distance and search origin. |
| Response and tidying | Compare successful empty results, malformed success bodies, absent fields, explicit nulls and HTTP failures. Check types and named columns on empty and populated returns. Trace nested addresses, contacts, categories, languages, coordinates and distance through tidying for loss or row multiplication. |
| Interpretation | Establish whether distance is reported or calculated, its units and origin. Check what a service listing supports. Current availability, eligibility, exhaustive coverage, access to care and unique clinic counts each need their own evidence. |

For material external claims, record the source, retrieval date, relevant passage, endpoint or service, and what remains unknown. Start with official HIV.gov Locator material and documentation available to the project. Distinguish documented guarantees, dated observations, package choices and unresolved questions. A single successful lookup cannot establish completeness or global uniqueness.

For an example intended for research, try to reconstruct the query from its description. Record retrieval date, service filters, location, radius and units, package version, and rules for combining records. Determine whether a second analyst could reproduce the transformation on the same input. For repeated live retrievals, distinguish changes in directory contents from changes in package behavior.

## 3. Independently exercise the package

Run checks in a disposable copy. Inspect startup files, test helpers, vignette chunks and CI guards before executing. Use a temporary home and temporary R startup files for child processes. Remove inherited CDC_LOCATOR_CLIENT_ID and CDC_LOCATOR_CLIENT_TOKEN and explicitly set CDC_LOCATOR_LIVE_TESTS=false for offline work. Supply dummy credentials only where mocks need them. Point `R_ENVIRON_USER` at the disposable file before any persistence check, including in a child process. Use public example locations or synthetic coordinates.

Build small synthetic HTTP responses with hand-worked expectations. Mock at the request boundary so ordinary construction, parsing, combination and tidying still execute. One useful integration case is a location resolving to a leading-zero ZIP followed by a multi-batch provider search. Vary the second batch between empty, repeated records and failure. Predict row counts, keys, types and error behavior before running. Preserve any serious discrepancy as a minimal rerunnable test or explain exactly what blocks reproduction.

Run focused checks first, then the offline suite, a clean source build, installed-package checks and the site build when feasible. Record commands, R, OS and dependency versions, skips and warnings. Inspect CI triggers, tested revisions, permissions, live-test gating, redaction, dependency and Windows coverage, and whether failing tests or renders actually fail the job. Distinguish personally run checks, observed CI and author reports. Keep live checks limited to specific unresolved questions and the authorized environment. Record sanitized request scope, date and outcome without publishing credentials, private addresses or sensitive logs.

## 4. Review the site as a user and verify it against the package

The documentation and accessibility reviewer owns coverage of the home page, getting started, every article, each exported function’s reference page, indexes, navigation, search, news and every distinct figure. Inventory the published pages as well as README.Rmd/README.md, vignettes/, man/, _pkgdown.yml and figure-generating sources. Reconcile missing, orphaned, duplicated and unintentionally published pages. Inspect Markdown or machine-readable copies when present. For overlapping explanations, specify which page should own the material and where a link suffices.

### First-time reader tasks

Start each reader at the home page with a goal. Keep the expected outcome, source code, direct article links and troubleshooting hints with the coordinator until the reader records an answer. Capture route, stopping point, missing prerequisite, help needed and interpretation. A task can produce working code and still fail because the reader draws the wrong conclusion.

| Brief given to the reader | What the coordinator checks after the attempt |
| --- | --- |
| Find what you need for your first search and how to tell whether the session is ready. | Can the reader find installation and access prerequisites, distinguish session from persistent setup, and check readiness without printing secrets? |
| Starting with a public city or point, find two service categories and explain what the distance means. | Can the reader discover the location-to-ZIP step, preserve the ZIP, choose services and radius, and identify the origin and units with appropriate uncertainty? |
| Compare two ZIP-based searches and explain how you would count distinct service records and distinct sites. | Can the reader distinguish service records from physical sites, explain a proposed grouping rule and its limitations, and preserve search origins where needed? |
| A search returned no rows yesterday and failed today. Decide what can be reported about each search. | Can the reader distinguish a successful empty result from input, credential, network or service failures and locate the relevant recovery advice? |

Use synthetic or sanitized offline examples to exercise steps that would otherwise need credentials. Record which steps a reader could actually complete from the published materials. Do not credit an unpublished fixture, coordinator hint or skipped code chunk as a successful public walkthrough.

### Page-by-page checks

1. Trace each substantive promise, displayed result and example back to its function, fixture, source or clearly labeled observation. Compare arguments, defaults, units, row meaning and failure messages. Check skipped and illustrative chunks explicitly. A successful render may have executed none of the displayed API calls.
2. Walk the home page, getting-started route, articles, reference and search in a browser. Verify links, anchors, menu grouping, descriptive labels and discoverability. Check desktop and phone-width behavior, keyboard navigation, focus, heading order, table overflow and code accessibility.
3. Build pkgdown from the frozen copy in the isolated environment. Compare the local render with the deployed site. Record source inspection, build result, local browser observations and deployed browser observations separately. If provenance or browser access is unavailable, identify exactly which conclusions are still open.

### Inspect every figure in context

Include inline and generated plots, maps, diagrams, SVGs, screenshots and interactive graphics. Discover visuals from both rendered pages and source, including assets generated at build time. Reuse one assessment for an identical asset but check each placement for caption, size and context. If there are no figures, record which pages and render were inspected. Identify a missing illustration only when it would resolve a concrete reader difficulty.

| Check | Evidence to record |
| --- | --- |
| Meaning and provenance | Page, figure, intended takeaway, source file or generating chunk, data provenance, and whether content is synthetic, recorded or current. Trace counts, denominators, scales, distance units, keys and arrows to the example or implementation. For maps, verify what points, boundaries and coverage areas represent. |
| Actual appearance | Open the rendered page at desktop and narrow phone widths and with zoom. Inspect labels, arrows, legends, clipping, overlap, contrast, symbol distinctions and image sharpness. Verify interactive controls and fallback content where applicable. |
| Equivalent explanation | Read caption, nearby prose and alt text without the image. Can the reader recover the intended relationship, trend or limitation? For complex figures, provide an accessible description or data table as appropriate. Check consistency when the figure and prose make the same claim. |
| Disposition | Record page anchor, source locator, viewports, result and concrete repair or evidence gap. A loaded image, valid SVG, nonempty alt attribute or passing link checker alone establishes little about readability. |

## 5. Reconcile and deliver

Verify findings against the frozen revision. Merge duplicates across code, help and figures while keeping distinct consequences. Check each open issue against current behavior, related PRs and prior resolutions. Record current evidence, next action and disposition. Refresh the issue list at close. Where a choice affects the public contract, present the options, downstream consequences, recommendation and acceptance check for the maintainer.

Deliver a concise assessment, the coverage table, reader-task and figure results, validation log, and issue-by-issue dispositions. Group findings under **Must fix**, **Should fix** and **Consider**. Give each a stable ID, pinned file and line or page anchor, practical impact, evidence, proposed remedy and acceptance check. Identify verified defects, documentation gaps, unverified concerns, and maintainer decisions. Preserve reusable regression examples alongside the report and identify the existing test, help page, article or CI job that should own each accepted fix.

Report substantive gaps as partial coverage, including a blocked render, uninspected figures, or unresolved live behavior. End with a one-line verdict scoped to what was actually assessed. Present findings before any separately authorized repository edits, issue posting or PR review.

### Sources for this protocol

- [sRtr whole-package review](https://github.com/VagishHemmige/sRtr/blob/2c845f89805754f58fa4043a23e1409075ab4099/PACKAGE_REVIEW.md)
- [R analysis workflow review modules](https://github.com/VagishHemmige/r-analysis-workflow/blob/13d6077b5c3e4846c7005260aac9442494e42da5/plugin/skills/review-analysis/SKILL.md)
- [R analysis workflow self-tests, including blind review and user simulation](https://github.com/VagishHemmige/r-analysis-workflow/blob/13d6077b5c3e4846c7005260aac9442494e42da5/testing/README.md)
