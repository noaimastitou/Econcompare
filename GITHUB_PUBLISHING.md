# Updating econcompare on GitHub

This folder is prepared to replace the contents of the existing repository:

`https://github.com/noaimastitou/Econcompare`

The repository already exists. Do **not** create a new repository and do not overwrite the hidden `.git` directory in an existing local clone.

## 1. Replace the repository files

If you use the GitHub web interface, upload the files and folders from this package root and replace files with the same names. Keep the repository structure unchanged, especially `R/`, `man/`, `tests/`, `inst/` and `.github/`.

If you use a local Git clone, copy the contents of this folder into the clone, replacing old files. Do not copy a `.git` directory from another location.

Then review the changes:

```bash
git status
git diff --stat
```

## 2. Maintainer metadata

`DESCRIPTION` deliberately still contains a placeholder maintainer identity/email because no real maintainer identity was supplied during package preparation:

```text
Authors@R: person("econcompare", "contributors", role = c("aut", "cre"), email = "maintainer@example.com")
```

Replace this with the real maintainer name and email before a CRAN or other formal registry submission. Do not invent or publish personal metadata you do not want public.

The repository URL and bug-report URL are already set to:

```text
URL: https://github.com/noaimastitou/Econcompare
BugReports: https://github.com/noaimastitou/Econcompare/issues
```

## 3. Test locally before publishing a stable release

From RStudio, open the package directory and run:

```r
install.packages(c("devtools", "testthat"))
devtools::document()
devtools::test()
devtools::check()
```

Then install and launch the package:

```r
devtools::install()
library(econcompare)
eco_app(mtcars)
```

Version 0.11.0 adds ECM, VAR and VECM, so also exercise the advanced time-series paths with `urca` and `vars` installed.

## 4. Commit and push

After reviewing `git status` and the local checks:

```bash
git add .
git commit -m "Release 0.11.0: ECM VAR and VECM support"
git push origin main
```

If the repository uses a different default branch, replace `main` accordingly.

## 5. Check GitHub Actions

Open the repository **Actions** tab after the push. The included `R-CMD-check` workflow is an additional software-quality gate. Review errors, warnings and notes rather than treating a green badge as econometric validation.

## 6. External installation

Once pushed, users can install the GitHub version with:

```r
install.packages("remotes")
remotes::install_github(
  "noaimastitou/Econcompare",
  dependencies = TRUE
)
```

Then:

```r
library(econcompare)
eco_app(mtcars)
```

## 7. Versioned release

Only after the checks are satisfactory, create the tag:

```bash
git tag -a v0.11.0 -m "econcompare 0.11.0 advanced time-series release"
git push origin v0.11.0
```

Then use **GitHub → Releases → Draft a new release**, select `v0.11.0`, and summarise the changes from `NEWS.md`.

## 8. Important validation limitation

This prepared package was assembled and statically audited in an environment without R/Rscript. An actual `R CMD check` could therefore not be run here. Your local `devtools::check()` and the GitHub Actions workflow remain required before describing v0.11.0 as runtime-tested or stable.
