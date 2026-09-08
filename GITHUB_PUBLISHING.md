# Publishing econcompare on GitHub

This folder is structured as the root of the GitHub repository.

## 1. Personalise repository metadata

Before the first public push, replace `YOUR_GITHUB_USERNAME` in:

- `README.md`
- `CITATION.cff`

Also update `DESCRIPTION` with your real maintainer name and email. Do not publish the placeholder maintainer address as your final package metadata.

A typical `Authors@R` field looks like:

```text
Authors@R: person("Noaïm", "Astitou", role = c("aut", "cre"), email = "noaimastitou.pro@gmail.com")
```

You may also add these fields to `DESCRIPTION`:

```text
URL: https://github.com/noaimastitou/econcompare
BugReports: https://github.com/noaimastitou/econcompare/issues
```

## 2. Test locally before publishing

From RStudio, open the package directory and run:

```r
install.packages(c("devtools", "testthat"))
devtools::document()
devtools::test()
devtools::check()
```

Do not treat the repository as release-ready until `devtools::check()` completes without errors. Review warnings and notes individually.

Then install the local package and test the interactive application:

```r
devtools::install()
library(econcompare)
eco_app(mtcars)
```

## 3. Create the GitHub repository

On GitHub:

1. Create a new repository named `econcompare`.
2. Prefer a public repository if external users should install it directly.
3. Do **not** initialise it with another README, `.gitignore` or licence, because this package folder already contains them.

## 4. Push using Git

Open a terminal in the `econcompare` folder and run:

```bash
git init
git add .
git commit -m "Initial public beta of econcompare"
git branch -M main
git remote add origin https://github.com/YOUR_GITHUB_USERNAME/econcompare.git
git push -u origin main
```

Alternatively, GitHub Desktop can be used to create a repository from this existing local folder and publish it.

## 5. Check GitHub Actions

After the push, open the repository's **Actions** tab. The included `R-CMD-check` workflow runs package checks on current release R for Linux, Windows and macOS.

A green workflow does not replace methodological validation, but it is a useful software-quality gate for future changes.

## 6. External installation

Once the repository is public, users can install it with:

```r
install.packages("remotes")
remotes::install_github(
  "YOUR_GITHUB_USERNAME/econcompare",
  dependencies = TRUE
)
```

Then:

```r
library(econcompare)
eco_app(mtcars)
```

## 7. Create a versioned GitHub release

When the repository is stable enough for a public beta release:

```bash
git tag -a v0.6.0 -m "econcompare 0.6.0 public beta"
git push origin v0.6.0
```

Then use **GitHub → Releases → Draft a new release**, select `v0.6.0`, and summarise the changes from `NEWS.md`.

## 8. Recommended repository settings

For a public research-software project:

- enable Issues for bug reports and methodological suggestions;
- enable Discussions if you want longer methodological conversations outside Issues;
- protect the `main` branch once collaborators begin contributing;
- require the R-CMD-check workflow before merging pull requests when the project becomes collaborative.

## 9. Important limitation of this prepared version

This package structure was assembled in an environment without an R installation, so an actual local `R CMD check` could not be run before delivery. Your local `devtools::check()` and the included GitHub Actions workflow are therefore mandatory validation steps before presenting v0.6.0 as a tested release.
