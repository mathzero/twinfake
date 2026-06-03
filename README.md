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
pipeline logic. Use `copy` only after explicit review.

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

`.xlsx` support is optional and uses `readxl` and `writexl` when installed.
All sheets are read by default, sheet names are preserved, and a single fake
workbook is written.

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
default, and does not display raw values by default.

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
