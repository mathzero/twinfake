# twinfake

`twinfake` creates privacy-first "pipeline twin" data for local code
development. It mirrors file names, folder structure, column names, data types,
row counts, missingness, dirty values, broad distributions, deterministic
relationships, and cross-file join behaviour so analysts can build and debug
pipelines against fake files.

The generated data are not anonymised, de-identified, differentially private, or
safe for public release. They are intended for private code development and
LLM-assisted pipeline construction. Review generated files before sending them
to any external system, and use a formal disclosure-risk review before any
release.

Column names and file names are preserved by default for pipeline compatibility.
Those names can themselves be sensitive, so review them before sharing.

## Installation

```r
install.packages("devtools")
devtools::install()
```

## Folder workflow

```r
library(twinfake)

make_fake_folder(
  input_dir = "data/private",
  output_dir = "data/fake",
  seed = 20260603,
  overwrite = TRUE
)
```

This recreates a parallel folder tree under `data/fake`. Supported files are
generated as fake equivalents. Unknown files are skipped by default and recorded
in `twinfake_manifest.json`.

## Data-frame workflow

```r
fake <- make_fake_data(
  real_data,
  seed = 20260603
)

validate_fake_data(real_data, fake)
```

All columns are sensitive by default. Sensitive categorical labels are replaced
with fake labels, while frequencies and duplicate patterns are preserved.

## Sensitivity specs

Use `public_code` only for labels that are genuinely safe and required by
pipeline logic. Use `permute` when private pipeline code needs the original
value set and marginal frequencies but row-level associations should be broken.
Use `copy` only after explicit review. Both `permute` and `copy` retain real
values and are deliberate disclosure-risk options.

Column actions:

- `sensitive`: generate synthetic values of the same broad type. Preserves row
  count, missingness, duplicate patterns, category frequencies, broad
  numeric/date distributions, and stable fake keys for key-like columns.
- `public_code`: reuse original non-missing labels as allowed categories.
  Preserves public label sets, observed frequencies, and missingness.
- `permute`: shuffle existing values across rows when row count is unchanged.
  Detected child columns reuse the same shuffle unless the child action is
  `drop` or `structure_only`. Retains real values, marginal distributions, and
  opted-in linked pairs.
- `copy`: copy original values in original row order. Retains raw values and
  row-level associations.
- `drop`: replace values with typed missing values while keeping the column in
  the output schema.
- `hash`: replace values with salted deterministic hashes. Equal input values
  produce equal hashes for the same salt.
- `structure_only`: replace non-missing values with simple placeholders such as
  `TEXT_PLACEHOLDER`, `LEVEL_PLACEHOLDER`, `0`, or `FALSE` while keeping type
  and missingness.

```r
spec <- list(
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
        review_decision = list(sensitivity = "permute"),
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

write_twin_spec(spec, "twinfake_spec.json")
fake <- make_fake_data(real_data, spec = spec)
```

## Excel files

`.xlsx` and `.xls` input support is optional and uses `readxl` and `writexl`
when installed. All sheets are read by default, sheet names are preserved, and a
single fake workbook is written.

Legacy `.xls` inputs are read, but fake workbooks are written as `.xlsx` files
with the same base name because `writexl` does not write old BIFF `.xls`
workbooks. The manifest records this conversion.

Formulas, styles, comments, hidden sheets, cell formatting, merged cells, and
workbook-level metadata are not guaranteed to be preserved in this first
implementation. Original workbooks are never silently copied.

```r
make_fake_folder(
  "data/private",
  "data/fake",
  xlsx_sheets = "all",
  overwrite = TRUE
)
```

## Local Shiny app

```r
launch_twin_app(
  input_dir = "data/private",
  output_dir = "data/fake"
)
```

The app requires optional packages `shiny` and `DT`, binds to `127.0.0.1` by
default, and does not display raw values by default. After scanning a folder,
select one or more rows in the Columns table, choose a column action, and apply
it to update those features together. The Relationships tab shows perfectly
tied columns that may be regenerated together. A relationship shown as
`shared permutation` means the parent column is permuted and the child column
reuses that same row shuffle before applying its own action.

## Privacy scan

```r
privacy_scan(
  "data/fake",
  forbidden_values = c("example-name", "example-id"),
  real_data = real_data
)
```

The scan returns file, column, token type, and severity. It does not return the
literal forbidden token.

## Limitations

`twinfake` prioritises code-development utility over analytic validity. Numeric
distributions are approximate, advanced dependency modelling is limited, and
optional formats require optional packages. `engine = "synthpop"` requires
`synthpop` and should be used only after reviewing disclosure risk.
