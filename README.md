# cohortTdd

[![R-CMD-check](https://github.com/bill-baumgartner/cohortTdd/workflows/R-CMD-check/badge.svg)](https://github.com/bill-baumgartner/cohortTdd/actions)
[![Codecov test coverage](https://codecov.io/gh/bill-baumgartner/cohortTdd/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bill-baumgartner/cohortTdd?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![OHDSI](https://img.shields.io/badge/OHDSI-collaborative-blue.svg)](https://www.ohdsi.org/)

Test-driven development for OHDSI cohort definitions. This package provides a simple, powerful framework for validating cohort logic against synthetic patient data with automated CI/CD integration.

## Installation

```r
# Install development version from GitHub (when published)
# devtools::install_github("your-org/cohortTdd")

# Or install locally
devtools::install_local("path/to/cohortTdd")
```

## Quick Start

### Option 1: Direct Validation (Simplest)

Test your cohort immediately with one function call:

```r
library(cohortTdd)

# Test your cohort definition
results <- validate_my_cohort(
  patients_file = "my_patients.xlsx",
  cohort_file = "my_cohort.json"
)
```

### Option 2: Set Up Testing Project (For CI/CD)

Create a complete testing project with GitHub Actions:

```r
library(cohortTdd)

# Create a new testing project
setup_cohort_testing_project(
  project_name = "my-diabetes-cohort-test",
  cohort_name = "diabetes_cohort",
  author_name = "Jane Doe"
)
```

This creates a project with:
- Template test files
- GitHub Actions workflow  
- Example patient data
- README with instructions

### Test Data Format

Your Excel file should contain:
- **Standard CDM tables**: person, observation_period, drug_exposure, measurement, etc.
- **explanation tab**: Required tab with columns:
  - `patient_id`: Patient identifier
  - `reason_id`: Expected exclusion reason ID (NA for included patients)
  - `explanation`: Free text description of expected outcome

## For CI/CD Integration

Wrap the validation in a testthat test using R package conventions:

```r
# tests/testthat/test_my_cohort.R
library(testthat)
library(cohortTdd)

test_that("My cohort validates correctly", {
  patients_file <- system.file("testdata", "my_patients.xlsx", package = "my-cohort-validation")
  cohort_file <- system.file("testdata", "my_cohort.json", package = "my-cohort-validation")
  
  results <- validate_my_cohort(
    patients_file = patients_file,
    cohort_file = cohort_file,
    print_summary = FALSE
  )
  
  expect_equal(results$failed_tests, 0,
    info = sprintf("Cohort validation failed for %d patients", results$failed_tests))
})
```

### Running Tests

```r
# Run validation directly
validate_my_cohort("patients.xlsx", "cohort.json")

# Run as part of package tests
devtools::test()
```

## Features

- **Zero-configuration testing**: Just provide Excel file + cohort JSON
- **Automatic patient discovery**: Reads all patients from explanation tab
- **Detailed validation**: Compares expected vs actual exclusion reasons
- **Comprehensive reporting**: Clear pass/fail summary with error details
- **CI/CD integration**: Works with standard R testing frameworks

## Example Output

```
======================================================================
                       COHORT VALIDATION SUMMARY
======================================================================
Total patients tested: 7
Tests passed: 6
Tests failed: 1
Success rate: 85.7%

❌ FAILED TESTS:
--------------------------------------------------
Patient 3: EXCLUDE - due to estradiol exposure
  Error: Patient 3 excluded with wrong reason: expected 2, got 1

📋 DETAILED RESULTS:
--------------------------------------------------
✅ Patient 1: INCLUDE - Exact match
✅ Patient 2: EXCLUDE - testosterone exposures <7 days apart
❌ Patient 3: EXCLUDE - due to estradiol exposure
✅ Patient 4: EXCLUDE - no HCT before index date
✅ Patient 5: EXCLUDE - <4 HCT after index date
✅ Patient 6: INCLUDE - Exact match
✅ Patient 7: INCLUDE - Exact match
```

## Dependencies

- TestGenerator: Creates synthetic CDM data
- CDMConnector: Manages cohort definitions and execution
- CirceR: Converts ATLAS cohort JSON to SQL
- readxl/writexl: Excel file handling
- dplyr: Data manipulation

## License

MIT