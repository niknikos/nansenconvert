# nansenconvert — Nansen Converter

A Shiny app that converts survey data to the NMD formats StoX reads:
**NMDBiotic v3.1** for trawl and biology data and **NMD Echosounder v1** for
acoustic NASC. Part of the EAF-Nansen tool suite.

| Tab | Converts |
|---|---|
| BioData_To_Biotic.xls | Stations, catches, length-frequency and biology sheets → `Biotic.xlsx` |
| Biotic.xls_To_Biotic.xml | `Biotic.xlsx` → `Biotic.xml` (NMDBiotic v3.1) |
| NASC.xls_To_Acoustic.xml | Echoview NASC export → `Acoustic.xlsx` |
| Acoustic.xls_To_Acoustic.xml | `Acoustic.xlsx` → Acoustic XML (NMD Echosounder v1) |
| Code lists | Nansen (NMD reference) or custom code lists for every coded field |
| Excel file templates | Synthetic example inputs |

Both XML tabs validate the data before download: keys, codes, plausibility
checks adapted from the Nansen Biotic Explorer, taxonomy, the official NMD
schemas and a read-back with RstoxData.

## Run

Requires R 4.6. From the project folder:

```r
shiny::runApp()
```

Packages: shiny, bslib, shinyjs, readxl, writexl, dplyr, lubridate, DT, xml2,
purrr, fontawesome, rhandsontable, stringi, shinycssloaders, shinyalert,
tibble, readr, later, worrms, RstoxData, and the EAF-Nansen libraries
**nansencore** and **nansentaxonomy** (from
<https://git.imr.no/cdcf/nansen-toolsuite>).

## Layout

- `app.R` — combines the converters into one app
- `*.R` in the root — the four converters
- `helpers/` — code lists, taxonomy (nansentaxonomy) and validation
- `schemas/` — the NMD XML schemas used for validation
- `data/` — synthetic templates, rebuilt by `data-raw/make_synthetic_templates.R`
- `docs/` — the user guide: `User_Guide.qmd` is the source of the `User_Guide.pdf`
  the app serves; render it with `quarto render docs/User_Guide.qmd`
- `tests/` — testthat suite: `Rscript tests/run_tests.R`
- `tests/browser_e2e.R` — runs the templates through every tab in headless Chrome
  and checks the downloads; with `--screenshots docs/img` it also refreshes the
  guide's screenshots

## Status

Developed on GitHub for now; it will move to the Nansen tool-suite area of
the IMR GitLab (`git.imr.no/cdcf/nansen-toolsuite/nansenconvert`).

## Credit

Based on the LPA **Excel XML Converter** (2026) by KMZ.
