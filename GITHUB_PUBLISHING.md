# Validating and publishing econcompare 0.14.4

The maintainer metadata is set to Noaïm ASTITOU, noaimastitou.pro@gmail.com.
The source folder is the repository root. No publication has been performed.

The current release gate and audit-fix mapping are in VALIDATION_0.14.4.md.

## Required local validation

Open the extracted econcompare directory in RStudio. Verify the source version,
not just the installed package version. Run:

```r
stopifnot(read.dcf("DESCRIPTION")[1, "Version"] == "0.14.4")
install.packages(c("devtools", "testthat", "tibble", "vars", "urca",
                   "sandwich", "lmtest", "shiny", "plm", "survival", "fixest"))
devtools::test()
devtools::check()
```

Install any other missing suggested packages reported by check. Investigate all
errors, warnings, notes and skipped stabilization tests. Rd documentation is maintained directly; document() is not required to run these tests.
If regenerating documentation, review the resulting diff.

## Install and verify the application

```r
devtools::install(upgrade = "never")
# Restart R before loading the newly installed package.
library(econcompare)
stopifnot(packageVersion("econcompare") == "0.14.4")
d <- read.csv(system.file("extdata", "econcompare_temporal_silicon_sample.csv",
                          package = "econcompare"))
d$date <- as.Date(d$date)
eco_app(d)
```

Check the panel tutorial and Shiny flow, the eight original linear estimators and all three new adapters, each with its own outcome family, absorption, singleton removal, cluster inference and diagnostic unavailable states. Also check ECM, VAR, VECM, Johansen labels, companion roots, serial-correlation output,
and errors for constant/collinear systems. A diagnostic rejection is not itself
an implementation failure. Confirm that no coefficient silently disappears.

## CI and release

The workflow includes Windows/macOS release and Linux release/oldrel/devel.
It explicitly requires the stabilization-test dependencies. Run the workflow on
the actual repository and inspect the results before making a release.

Only after local checks, application checks and CI are accepted, create the
v0.14.4 tag and GitHub release. Set date-released in CITATION.cff to the actual
publication date. Do not describe this source delivery as a certified release:
R was unavailable in the preparation environment, so no R test suite or
R CMD check result for 0.14.4 was obtained there.

## Uploading this folder to GitHub

The contents of the `Econcompare` folder should be placed directly at the root of the `noaimastitou/Econcompare` repository. Files and directories such as `DESCRIPTION`, `R/`, `man/`, `inst/`, and `tests/` should therefore be directly visible at the repository root.

Do not upload the ZIP archive itself, and do not create an additional `Econcompare/` subdirectory inside the repository.

Keep `.github/`, `.Rbuildignore`, and `.gitignore`.

Because the folder contains more than 100 files, uploading through the GitHub web interface may require two uploads to the same branch. For example:

1. Upload `R/`, `man/`, `inst/`, and `tests/`.
2. Upload the remaining files together with `.github/`.

Alternatively, GitHub Desktop can be used to clone the repository locally, copy the complete project contents into the clone, and prepare a single commit.

Before releasing version `v0.14.4`, run the validation checks described above and verify the GitHub Actions results.

The code may be pushed to a validation branch before those checks are completed. Doing so should not be interpreted as certification of stability or as a CRAN release.

GitHub documentation for uploading files:

https://docs.github.com/en/repositories/working-with-files/managing-files/adding-a-file-to-a-repository
