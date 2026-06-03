Create an R package for generating privacy-preserving dummy data sets that are safe to use for LLM/Codex-assisted code development.

Working package name: twinfake

Core product idea:
Build a package that creates “pipeline twin” data: fake data that mirrors the structure, column names, file names, folder structure, data types, missingness, dirty values, broad distributions, dependency structure, deterministic relationships, and cross-file join behaviour of sensitive real data. The purpose is not to create analytically valid public-use synthetic data. The purpose is to let analysts develop preprocessing, validation, import, cleaning, wrangling, modelling, and reporting code against fake files without sending real data to an LLM.

The package must run locally in R. It must never call an external API, never send data over the network, and never write real values into logs, metadata, reports, package examples, test snapshots, temporary files, or synthetic outputs unless the user explicitly marks specific columns as non-sensitive or public-code columns.

Important privacy stance:
1. Treat all columns as sensitive by default.
2. Preserve column names and file names by default because the analyst needs painless switching between real and fake data roots, but warn that names themselves may be sensitive.
3. Do not claim that generated data are anonymised, de-identified, differentially private, or safe for public release.
4. State clearly in README and help pages that the output is intended for private code development and LLM-assisted pipeline construction, not release without formal disclosure-risk review.
5. Avoid copying real categorical levels by default. Generate fake levels with matching cardinality and frequencies. Provide an explicit option for public codes when preserving original category labels is necessary for code compatibility.

Main user workflow:
The analyst should be able to run something like:

library(twinfake)

make_fake_folder(
  input_dir = "data/private",
  output_dir = "data/fake",
  seed = 20260603,
  overwrite = TRUE
)

This should recursively scan the input folder, process supported data files, and recreate a parallel folder tree under output_dir with the same relative paths and same file names wherever technically possible. The analyst should be able to switch pipelines by changing only the data root from "data/private" to "data/fake".

Primary exported functions:
1. make_fake_folder()
2. make_fake_data()
3. profile_data()
4. profile_folder()
5. write_twin_spec()
6. read_twin_spec()
7. launch_twin_app()
8. validate_fake_data()
9. privacy_scan()

Implement roxygen2 documentation for every exported function.

Function design:

make_fake_folder(
  input_dir,
  output_dir,
  spec = NULL,
  seed = NULL,
  overwrite = FALSE,
  recursive = TRUE,
  include = NULL,
  exclude = NULL,
  sensitivity = "all",
  preserve_file_names = TRUE,
  preserve_row_count = TRUE,
  xlsx_sheets = "all",
  unknown = c("skip", "empty_placeholder", "copy"),
  engine = c("pipeline", "synthpop", "independent"),
  risk_level = c("strict", "balanced", "utility"),
  report = TRUE,
  quiet = FALSE
)

make_fake_data(
  x,
  spec = NULL,
  seed = NULL,
  sensitivity = "all",
  preserve_row_count = TRUE,
  engine = c("pipeline", "synthpop", "independent"),
  risk_level = c("strict", "balanced", "utility"),
  quiet = FALSE
)

profile_data(
  x,
  sensitivity = "all",
  public_codes = NULL,
  key_cols = NULL,
  deterministic = NULL,
  risk_level = c("strict", "balanced", "utility")
)

profile_folder(
  input_dir,
  recursive = TRUE,
  include = NULL,
  exclude = NULL,
  sensitivity = "all",
  public_codes = NULL,
  key_cols = NULL,
  risk_level = c("strict", "balanced", "utility")
)

write_twin_spec(spec, path)
read_twin_spec(path)

launch_twin_app(
  input_dir,
  output_dir = NULL,
  spec_path = NULL,
  host = "127.0.0.1",
  port = NULL
)

validate_fake_data(real, fake, spec = NULL)
privacy_scan(output_dir, forbidden_values = NULL, real_data = NULL)

Package dependencies:
Use a small import footprint.

Imports:
- cli
- fs
- jsonlite
- rlang
- tibble
- vctrs
- stats
- utils
- tools
- methods

Suggests:
- readr
- data.table
- readxl
- writexl
- haven
- arrow
- qs
- synthpop
- ranger
- shiny
- DT
- testthat
- withr
- waldo
- knitr
- rmarkdown

Use suggested packages only when installed. Emit clear errors through cli::cli_abort() when a requested file type needs a missing suggested package.

Do not make Shiny a hard dependency. launch_twin_app() should check that shiny and DT are installed.

File support:
Implement a file-handler registry so new formats can be added later.

Initial handlers:
1. .csv
2. .tsv
3. .txt delimited files
4. .rds
5. .RData and .rda
6. .xlsx
7. .parquet if arrow is installed
8. .feather if arrow is installed
9. .sav, .dta, .sas7bdat if haven is installed
10. .qs if qs is installed

For .xlsx:
- Read all sheets by default.
- Preserve sheet names.
- Read each sheet as a separate tabular data frame.
- Generate one fake data frame per sheet.
- Write a single output workbook with the same sheet names.
- Use readxl for reading and writexl for writing.
- Clearly document that formulas, styles, comments, hidden sheets, cell formatting, merged-cell structures, and workbook-level metadata are not guaranteed to be preserved in the first implementation.
- Do not silently copy original workbooks.

For unknown files:
- unknown = "skip": skip and record in manifest.
- unknown = "empty_placeholder": create a zero-byte or text placeholder with a warning in the manifest.
- unknown = "copy": copy the file only when the user explicitly requests this dangerous behaviour.

Folder behaviour:
- Preserve relative paths from input_dir to output_dir.
- Create output_dir if required.
- Refuse to overwrite unless overwrite = TRUE.
- Avoid writing inside input_dir unless output_dir is clearly separate.
- Produce a manifest file called twinfake_manifest.json in output_dir when report = TRUE.
- Manifest must include file paths, generated row counts, column names, classes, warning messages, package version, seed, engine, and risk_level.
- Manifest must not include real values, real factor levels, real examples, exact unique IDs, raw records, or model objects fitted on real data.

Data profiling:
Build internal S3 generics:
- profile_object()
- fake_object()
- profile_vec()
- fake_vec()
- restore_fake_class()

The profiler should handle:
- data.frame
- tibble
- data.table, preserving as data.frame/tibble unless data.table is installed
- atomic vectors
- factors
- ordered factors
- Date
- POSIXct
- POSIXlt fallback via conversion warning
- difftime
- logical
- integer
- double
- complex
- raw
- character
- list columns
- matrices and arrays
- nested lists
- arbitrary R objects through recursive fallback

For unsupported objects:
- Preserve broad structure, names, dimensions, and classes where possible.
- Replace values with safe placeholders.
- Emit a warning.
- Never copy unknown object internals by default if they may contain sensitive values.

Column profiling:
For every vector column, store enough aggregate metadata to regenerate similar fake values.

Do not store raw values unless the user explicitly marks the column as non-sensitive or public code.

For all columns:
- length
- class
- typeof
- names presence
- attributes where safe
- missingness proportion
- missingness state counts where relevant
- uniqueness rate
- duplicate rate
- run-length structure where useful
- sortedness
- constant-column status
- row-order dependency hints

Numeric columns:
- integer/double class
- zero proportion
- positive/negative proportions
- missing, NaN, Inf, -Inf counts
- roundedness and decimal-place distribution
- quantile grid
- robust centre and scale
- outlier proportions
- repeated-value pattern
- monotonicity and step-size pattern
- optional correlation metadata

In strict mode:
- Do not preserve exact min and max if they may be identifying.
- Use jittered or winsorised endpoints.
- Use binned or quantile summaries rather than raw values.

Date/POSIXct columns:
- class and timezone
- granularity: day, week, month, quarter, year, second, minute, hour
- quantile grid over numeric representation
- weekday distribution
- month distribution
- missingness
- sortedness
- repeated-value pattern
- preserve broad range, not exact rare dates, in strict mode

Categorical/factor columns:
- number of levels
- ordered status
- frequency distribution
- rare-level pattern
- missingness
- level-label strategy:
  - sensitive default: generate fake labels
  - public_codes: preserve original labels
  - copy: preserve original values only when explicitly requested
- Preserve factor levels as fake levels by default so code using factor operations works.
- Preserve ordered factor order using fake labels.

Character columns:
Profile strings by pattern rather than value:
- length distribution
- empty string rate
- whitespace-only rate
- leading/trailing whitespace rates
- case pattern: upper, lower, title, mixed
- character class pattern: alpha, numeric, alphanumeric, punctuation, space, symbol
- common separators without storing surrounding tokens
- numeric-looking strings
- date-looking strings
- ID-like strings
- postcode-like pattern, but do not preserve real postcodes
- email-like pattern, but do not preserve domains by default
- URL-like pattern, but do not preserve real domains by default
- dirty missing strings such as "NA", "N/A", "NULL", ".", "-", "unknown", blank-like tokens; preserve token class, not exact sensitive token, unless public_codes is set
- encoding/invalid UTF-8 detection where possible

Generate character values that preserve:
- string lengths
- structural patterns
- dirty blank/NA-like tokens
- leading zeros in ID-like values
- punctuation structure
- casing
- duplicate rates
- high-cardinality vs low-cardinality behaviour

Dirty data preservation:
The package must deliberately preserve impurities because the goal is pipeline development.

Preserve, approximately:
- NA patterns
- NaN, Inf, -Inf for numeric vectors
- blank strings
- whitespace-only strings
- leading/trailing whitespace
- mixed type strings inside character columns
- numeric-looking strings with commas, currency symbols, percent signs
- inconsistent date formats as strings
- duplicate records
- duplicate IDs
- invalid category tokens through fake invalid tokens
- sentinel values such as fake versions of impossible dates or impossible numeric values
- unexpected punctuation
- inconsistent case
- factor levels that are present but unused
- columns with all missing values
- columns with all identical values
- zero-row and zero-column data frames

Do not preserve exact sensitive dirty tokens by default. Preserve the class of dirtiness and the rate.

Dependency preservation:
Implement a dependency profiler and generator.

Dependency types:
1. Exact duplicate columns.
2. Deterministic transformations.
3. One-to-one categorical mappings.
4. Many-to-one categorical mappings.
5. Cross-file keys.
6. Numeric correlations.
7. Mixed numeric-categorical associations.
8. Missingness co-occurrence.
9. Row-order or time-order relationships.

Exact deterministic relationships:
Detect common transformations in memory only:
- identical vector
- lower-case version
- upper-case version
- trimmed version
- substring or prefix relationship
- suffix relationship
- simple acronym relationship
- paste/concatenation of other columns using common separators
- year/month/day extracted from Date or POSIXct
- age derived approximately from date of birth and reference date
- numeric rounding
- numeric scaling
- numeric binning

When detected, generate the source column first and derive the dependent column from the fake source. This is essential for columns such as name and short_name.

Categorical/text perfect correlation:
If two categorical or character columns are totally or near-totally correlated, preserve that relationship with fake levels.

Example:
Original:
name        short_name
"Alpha GP"  "ALP"
"Alpha GP"  "ALP"
"Beta PCN"  "BET"

Fake:
name          short_name
"entity_001"  "ent_001"
"entity_001"  "ent_001"
"entity_002"  "ent_002"

The real labels must not appear, but the mapping and duplicate structure must remain.

Cross-file keys:
Implement global key mapping across a folder run.

Examples:
- patients.csv has patient_id
- appointments.csv has patient_id
- lab_results.xlsx has patient_id in several sheets

The fake files must preserve join cardinality:
- same fake patient ID appears everywhere the same real patient ID appeared
- duplicate IDs remain duplicates
- orphan foreign keys remain orphans
- missing keys remain missing
- invalid key-like strings remain invalid fake key-like strings

Do not write the real-to-fake key map to disk. Keep it in memory only.

Provide manual key specification in spec:
key_cols = list(
  patient_id = c("patients.csv:patient_id", "appointments.csv:patient_id", "lab_results.xlsx:Sheet1:patient_id")
)

Also implement automatic key detection:
- same column name across files
- high uniqueness in one table
- repeated values in another table
- inclusion or high-overlap relationship
- names ending in "_id", "id", "_key", "code", "nhs_number", or similar
- allow users to review these suggestions in the Shiny app or spec file

Default engine:
Implement engine = "pipeline" as the default.

The pipeline engine should be privacy-first and designed for code development:
- Preserve schema exactly.
- Preserve row count by default.
- Preserve missingness and dirty patterns.
- Preserve exact deterministic and categorical mapping relationships using fake labels.
- Preserve numeric distributions approximately through quantile-based simulation.
- Preserve numeric correlations approximately through rank/Gaussian-copula style generation where feasible.
- Preserve categorical frequencies with generated fake labels.
- Preserve joint distributions exactly only for low-cardinality non-identifying combinations or when risk_level allows it.
- Avoid row-level copying.

Optional synthpop engine:
Add engine = "synthpop" only if synthpop is installed.
Use it carefully:
- Do not use it as the default.
- Do not allow original sensitive categorical labels to leak.
- Encode sensitive categorical values to fake levels before synthesis.
- Use synthpop for users who prefer sequential modelling.
- Add warnings about disclosure risk and the need for validation.

Independent engine:
Add engine = "independent" for a simple fallback:
- Generate each column independently from its profile.
- Preserve schema, distributions, and dirtiness.
- Do not attempt correlation preservation except exact deterministic columns and cross-file keys.

Sensitivity specification:
Create a spec object with per-column controls.

Column sensitivity classes:
1. sensitive: default; generate fake values and fake levels.
2. public_code: preserve category labels or code labels because they are required for pipeline logic.
3. copy: copy values exactly; dangerous and opt-in only.
4. drop: remove or replace the column with all NA/fake placeholders.
5. hash: produce deterministic salted hashes for keys where useful.
6. structure_only: preserve type and missingness but use placeholders.

Spec format:
Use a JSON-serialisable list. Provide write_twin_spec() and read_twin_spec().

Example spec:
list(
  defaults = list(
    sensitivity = "sensitive",
    engine = "pipeline",
    risk_level = "strict"
  ),
  files = list(
    "patients.csv" = list(
      columns = list(
        patient_id = list(role = "key", sensitivity = "sensitive"),
        sex = list(sensitivity = "public_code"),
        name = list(sensitivity = "sensitive"),
        short_name = list(sensitivity = "sensitive", derived_from = "name")
      )
    )
  ),
  keys = list(
    patient_id = list(
      columns = c("patients.csv:patient_id", "appointments.csv:patient_id")
    )
  )
)

Shiny app:
Build a local Shiny app as an optional interface.

Requirements:
- launch_twin_app() opens a local-only app.
- Default host must be 127.0.0.1.
- App lets the user select input_dir and output_dir.
- App shows the folder tree.
- App shows files, sheets, columns, classes, missingness, uniqueness, dirty-pattern summaries, and dependency/key suggestions.
- App should not display raw values by default.
- Add an explicit “show raw values locally” control only if needed, with a warning. Do not include raw values in saved specs.
- App lets the user mark columns as sensitive, public_code, copy, drop, hash, or structure_only.
- App lets the user accept/reject suggested cross-file keys and deterministic relationships.
- App writes a spec JSON file.
- App can run make_fake_folder() from the selected spec.

Privacy scanning:
Implement privacy_scan().

privacy_scan() should:
- Search generated files and reports for forbidden literal strings supplied by the user.
- Optionally derive a set of high-risk tokens from real_data in memory, such as unique long character values, names, emails, IDs, postcodes, URLs, and high-cardinality category labels.
- Check that those tokens do not appear in the output.
- Never write the forbidden token list to disk.
- Return a data frame of possible leaks with file, column, token_type, and severity.
- Include tests that confirm known sensitive fixture values never appear in generated outputs, specs, manifests, or reports.

Validation:
Implement validate_fake_data(real, fake, spec = NULL).

It should return a structured object with:
- schema_match
- column_names_match
- column_order_match
- class_match
- row_count_match
- missingness_similarity
- dirty_pattern_similarity
- numeric_distribution_similarity
- categorical_frequency_similarity
- dependency_similarity
- key_integrity
- deterministic_relationship_preservation
- possible_privacy_issues

Print method:
Implement print.twinfake_validation() with a concise summary.

Do not print sensitive values.

Package architecture:
Create these files:

R/
  make_fake_folder.R
  make_fake_data.R
  profile_data.R
  profile_folder.R
  spec.R
  file_handlers.R
  handlers-delimited.R
  handlers-rds.R
  handlers-rdata.R
  handlers-xlsx.R
  handlers-arrow.R
  handlers-haven.R
  handlers-qs.R
  profile-object.R
  profile-vector.R
  fake-object.R
  fake-vector.R
  dependencies.R
  deterministic.R
  keys.R
  dirty-patterns.R
  privacy-scan.R
  validation.R
  shiny-app.R
  utils-random.R
  utils-strings.R
  utils-checks.R
  utils-cli.R
  zzz.R

tests/testthat/
  test-make-fake-data.R
  test-folder-mirror.R
  test-xlsx.R
  test-dirty-patterns.R
  test-deterministic-relationships.R
  test-cross-file-keys.R
  test-privacy-scan.R
  test-validation.R
  test-spec.R
  test-unsupported-objects.R
  test-reproducibility.R

vignettes/
  twinfake-folder-workflow.Rmd
  twinfake-sensitivity-spec.Rmd
  twinfake-xlsx-workflow.Rmd
  twinfake-llm-codex-workflow.Rmd

README.md
NEWS.md
LICENSE
DESCRIPTION
NAMESPACE

Testing fixtures:
Create toy fixtures programmatically inside tests. Do not include real data.

Test fixture should include:
- patients table with patient_id, name, short_name, dob, age, sex, postcode, email, status, dirty_numeric_string, date_string, notes
- appointments table with patient_id as a foreign key, appointment_id, date, status, clinician_code
- lab table with patient_id, lab_name, lab_value, unit, abnormal_flag
- xlsx workbook with multiple sheets containing related keys
- exact name/short_name mapping
- dirty values such as blanks, whitespace, "N/A", impossible dates, numeric strings with commas, duplicate IDs, orphan keys, missing keys, mixed case, leading zero IDs
- known sensitive tokens such as "Alice Secret", "Bob Confidential", "NHS999999", "ZX99 1ZZ", "alice.secret@example.test"

Critical tests:
1. make_fake_data() preserves column names, order, row count, and broad classes.
2. make_fake_folder() recreates the folder tree.
3. .xlsx handler processes all sheets and preserves sheet names.
4. Output does not contain known sensitive tokens.
5. Manifest does not contain known sensitive tokens.
6. Spec file does not contain known sensitive tokens.
7. name and short_name remain perfectly mapped after faking.
8. Cross-file patient_id joins work after faking.
9. Missingness rates are within a reasonable tolerance.
10. Dirty-pattern rates are within a reasonable tolerance.
11. Numeric distributions have similar decile structure within a reasonable tolerance.
12. Public-code columns preserve allowed labels when explicitly configured.
13. Sensitive categorical columns do not preserve original labels by default.
14. Same seed gives identical output.
15. Different seeds give different output.
16. Unsupported object fallback does not leak values.
17. R CMD check passes.

Randomness:
Use deterministic, reproducible RNG handling.
- Accept seed.
- Use withr::with_seed() if withr is installed, or base set.seed() with careful state restoration.
- Avoid global RNG side effects where possible.
- Ensure folder runs are reproducible independent of operating-system file ordering by sorting paths before processing.

String generation:
Implement safe string generators:
- fake labels: paste0(safe_prefix, "_", zero-padded index)
- fake names: generic entity labels rather than realistic names by default
- fake emails: userNNN@example.invalid
- fake URLs: https://example.invalid/path/NNN
- fake postcodes: use structurally valid-looking but non-real placeholders such as AA1 1AA variants or clearly fake outward/inward patterns
- fake IDs: preserve leading zeros and length patterns
- fake dirty tokens: use safe placeholders such as "MISSING_TOKEN", "UNKNOWN_TOKEN", "INVALID_DATE", "BAD_NUMERIC" while preserving pattern rates

Do not use external faker packages as hard dependencies.

Numeric generation:
Implement quantile-based generation:
- Estimate quantile grid from observed non-missing finite values.
- Simulate uniform probabilities.
- Interpolate over quantile grid.
- Add jitter within bins.
- Restore integer class when required.
- Restore roundedness and decimal places.
- Reinsert NA, NaN, Inf, -Inf according to profiled state pattern.

For correlated numeric blocks:
- Compute rank-transformed pseudo-observations.
- Estimate Spearman correlation.
- Shrink non-positive-definite correlation matrices to a safe positive-definite approximation.
- Simulate multivariate normal values.
- Convert simulated normal values to uniform probabilities.
- Back-transform through each column’s quantile profile.
- Fall back to independent generation when the matrix is unstable.

Categorical generation:
For sensitive categories:
- Generate fake levels level_001, level_002, etc.
- Preserve number of levels and frequency distribution.
- Preserve unused levels for factors.
- Preserve ordered factor level order.
- Preserve duplicates.
- Avoid real labels.

For public_code:
- Preserve labels exactly.
- Still preserve missingness and frequencies.
- Add warning that public_code labels appear in fake output by design.

Missingness:
Profile missingness jointly.
- Generate a missingness-pattern matrix by resampling observed missingness signatures.
- Apply missingness after value generation.
- Preserve column-wise missingness and common co-missingness patterns.
- Include special numeric states separately: NA, NaN, Inf, -Inf.

Deterministic relationship implementation:
Build a dependency graph.
- Detect parent-child relationships.
- Topologically sort columns.
- Generate parents before children.
- Apply transformation to fake parent values.
- Avoid cycles; break cycles with clear warnings.
- For exact duplicate columns, copy fake parent values.
- For one-to-one sensitive categorical maps, create fake paired levels and apply the fake map.

Cross-file implementation:
During make_fake_folder():
1. Read/profiling pass over all supported files.
2. Detect or read key spec.
3. Build ephemeral key domains in memory.
4. Generate fake files using key domains.
5. Write outputs.
6. Drop real data and key maps from memory.
7. Write safe manifest and validation report.

The implementation may be memory-based for the first version. Add clear internal seams for future chunked/disk-backed processing.

Reports:
When report = TRUE:
- Write twinfake_manifest.json.
- Optionally write twinfake_validation.html or twinfake_validation.json.
- Report schema, dimensions, warnings, skipped files, unsupported types, validation scores, and privacy scan summary.
- Do not include raw real values, sample rows, original factor labels, original IDs, or model objects.

README content:
README must include:
- Package purpose.
- Installation instructions.
- Basic folder example.
- Data-frame example.
- xlsx example.
- sensitivity spec example.
- Shiny app example.
- Privacy warning.
- Limitations.
- Statement that fake outputs are for code development, not public release.
- Statement that generated data should be reviewed before being sent to external systems.

Development style:
- Use idiomatic R.
- Use clear S3 classes for profiles, specs, validation results, and folder results.
- Use cli for messages.
- Use rlang for argument validation where helpful.
- Avoid tidyverse-heavy code in internals unless necessary.
- Avoid non-standard evaluation in exported APIs unless there is a strong reason.
- Keep comments concise and active.
- Do not use hard-coded absolute paths.
- Do not use setwd().
- Do not use internet or network calls.
- Do not include real example data.
- Keep functions small and testable.
- Ensure R CMD check passes.

Acceptance criteria for first complete version:
1. Package installs with devtools::install().
2. devtools::test() passes.
3. devtools::check() passes with no errors, warnings, or notes where practical.
4. make_fake_data() works for a mixed tibble containing numeric, integer, logical, factor, ordered factor, character, Date, POSIXct, difftime, list column, and dirty values.
5. make_fake_folder() mirrors a nested folder with csv, rds, and xlsx files.
6. xlsx processing handles all sheets.
7. Default output contains no original sensitive values from the test fixture.
8. Exact text/categorical correlations are preserved with fake values.
9. Cross-file keys are preserved with fake IDs.
10. Public-code opt-in preserves configured labels.
11. The manifest is useful but contains no real sensitive values.
12. launch_twin_app() starts when shiny is installed and fails informatively when shiny is missing.

Suggested implementation order:
1. Scaffold the package.
2. Implement spec classes and argument checking.
3. Implement profile_vec() and fake_vec() for core atomic types.
4. Implement make_fake_data() for data frames.
5. Implement dirty-pattern profiling and generation.
6. Implement deterministic relationship detection for duplicate, trim, case, prefix, substring, and one-to-one categorical maps.
7. Implement validation and privacy_scan().
8. Implement file handlers for csv, rds, and xlsx.
9. Implement make_fake_folder().
10. Implement cross-file key handling.
11. Add optional handlers for arrow, haven, and qs.
12. Add optional synthpop engine.
13. Add Shiny app.
14. Write README and vignettes.
15. Run tests and check.
16. Fix all failures.

Start by creating the package files and implementing the first complete version. Prioritise correctness, privacy-safe defaults, test coverage, and a clean user API over advanced modelling.