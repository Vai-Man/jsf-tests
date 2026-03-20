## Test Solutions

### 1. Easy Test: `example.qmd`

Uses the Python `jsf` package in R through `reticulate` in a Quarto report.
- **View Live:** [https://vai-man.github.io/jsf-tests/example.html](https://vai-man.github.io/jsf-tests/example.html)
- **Run it:** Open `example.qmd` in RStudio and click "Render" (or run `quarto::quarto_render("example.qmd")`).
- **Features:** Simulates a Birth-Death process and a Lotka-Volterra predator-prey system with graphs and mean-field comparisons.

### 2. Medium Test: `jsfR` Package

An R package that wraps the Python JSF code using `reticulate`.
- **Run Unit Tests:** `devtools::test("jsfR")`
- **Compile Documentation:** `devtools::document("jsfR")`
- **Install it:** `devtools::install("jsfR")`
- **Features:** Loads Python modules safely, validates inputs, converts data between R and Python, and skips Python code on CRAN if needed.

### 3. Hard Test: Gillespie SSA Comparison

Compares an exact R simulator (Gillespie SSA) against JSF output to measure differences. Uses the `jsfR` package.
- **Run it:** 
  ```r
  # Ensure the package is loaded via library(jsfR) before sourcing
  source("hard_test.R")
  ```
- **Output:** Runs simulations, caches 3,000 Python JSF calls, saves comparison plots to `output/`, and creates a CSV file with results.

## Requirements

R `4.1+`, Python `3.8+`, and the following packages:
```r
install.packages(c("reticulate", "ggplot2", "testthat", "devtools"))
```
