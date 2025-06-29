#' Set up a new cohort testing project
#'
#' @param project_name Name of the project directory to create
#' @param path Parent directory where project should be created (default: current directory)
#' @param cohort_name Name of the cohort being tested (used in filenames)
#' @param author_name Author name for project files
#' @param github_actions Whether to include GitHub Actions workflow (default TRUE)
#' @return Path to created project directory
#' @export
#' @examples
#' \dontrun{
#' setup_cohort_testing_project(
#'   project_name = "my-diabetes-cohort-test",
#'   cohort_name = "diabetes_cohort",
#'   author_name = "Jane Doe"
#' )
#' }
setup_cohort_testing_project <- function(project_name, 
                                        path = ".", 
                                        cohort_name = "my_cohort",
                                        author_name = "Your Name",
                                        github_actions = TRUE) {
  
  # Input validation with custom error messages
  checkmate::assert_string(project_name)
  checkmate::assert_string(path)
  checkmate::assert_string(cohort_name, min.chars = 1)
  checkmate::assert_string(author_name, min.chars = 1)
  
  # Custom validation with expected error messages
  if (nchar(project_name) == 0) {
    stop("project_name cannot be empty")
  }
  
  if (!dir.exists(path)) {
    stop("Parent directory does not exist: ", path)
  }
  checkmate::assert_logical(github_actions, len = 1)
  
  project_path <- file.path(path, project_name)
  
  if (dir.exists(project_path)) {
    stop("Project directory already exists: ", project_path)
  }
  
  # Create directory structure
  dir.create(project_path, recursive = TRUE)
  dir.create(file.path(project_path, "inst", "testdata"), recursive = TRUE)
  dir.create(file.path(project_path, "tests", "testthat"), recursive = TRUE)
  
  if (github_actions) {
    dir.create(file.path(project_path, ".github", "workflows"), recursive = TRUE)
  }
  
  # Create template files
  create_template_description(project_path, project_name, author_name)
  create_template_readme(project_path, project_name, cohort_name)
  create_template_test(project_path, cohort_name)
  create_template_patients_excel(project_path)
  create_template_cohort_json(project_path, cohort_name)
  
  if (github_actions) {
    create_github_actions_workflow(project_path)
  }
  
  message("✅ Created cohort testing project at: ", project_path)
  message("📝 Next steps:")
  message("   1. Replace inst/testdata/patients.xlsx with your patient data")
  message("   2. Replace inst/testdata/", cohort_name, ".json with your cohort definition")
  message("   3. Update explanation tab in Excel with expected results")
  message("   4. Run: devtools::test()")
  
  return(project_path)
}

# Helper functions for creating template files
create_template_description <- function(project_path, project_name, author_name) {
  content <- sprintf('Package: %s
Type: Package
Title: Cohort Testing Project
Version: 0.1.0
Authors@R: person("%s", email = "your.email@example.com", role = c("aut", "cre"))
Description: Testing project for OHDSI cohort definitions.
License: MIT
Encoding: UTF-8
Depends: R (>= 4.1.0)
Imports:
    cohortTdd
Suggests:
    testthat (>= 3.0.0),
    devtools
RoxygenNote: 7.2.3
', gsub("-", ".", project_name), author_name)
  
  writeLines(content, file.path(project_path, "DESCRIPTION"))
}

create_template_readme <- function(project_path, project_name, cohort_name) {
  # Get the project package name from the directory name
  package_name <- gsub("-", ".", basename(project_path))
  
  content <- sprintf('# %s

This project tests the %s cohort definition using the cohortTdd package.

## Quick Start

1. **Replace test data**: Update `inst/testdata/patients.xlsx` with your patient data
2. **Replace cohort**: Update `inst/testdata/%s.json` with your cohort definition  
3. **Update expectations**: Modify the "explanation" tab in the Excel file
4. **Run tests**: `devtools::test()`

## Files

- `inst/testdata/patients.xlsx` - Patient test data with explanation tab
- `inst/testdata/%s.json` - Cohort definition from ATLAS
- `tests/testthat/test_%s.R` - Test that validates the cohort

## Quick Validation

```r
library(cohortTdd)

# Test your cohort directly
results <- validate_my_cohort(
  patients_file = system.file("testdata", "patients.xlsx", package = "%s"),
  cohort_file = system.file("testdata", "%s.json", package = "%s")
)
```

## CI/CD

This project includes GitHub Actions workflow for automated testing. 
Push to GitHub and tests will run automatically on every commit.

## Patient Data Format

Your Excel file should have:
- Standard CDM tables: person, observation_period, drug_exposure, measurement, etc.
- **explanation tab** with columns:
  - `patient_id`: Patient identifier
  - `reason_id`: Expected exclusion reason (NA for included patients)  
  - `explanation`: Description of why patient should be included/excluded

## Support

See the cohortTdd package documentation for more details.
', project_name, cohort_name, cohort_name, cohort_name, cohort_name, package_name, cohort_name, package_name)
  
  writeLines(content, file.path(project_path, "README.md"))
}

create_template_test <- function(project_path, cohort_name) {
  # Get the project package name from the directory name
  package_name <- gsub("-", ".", basename(project_path))
  
  content <- sprintf('library(testthat)
library(cohortTdd)

test_that("%s cohort validates correctly", {
  # Use system.file() to find test data (follows R package conventions)
  patients_file <- system.file("testdata", "patients.xlsx", package = "%s")
  cohort_file <- system.file("testdata", "%s.json", package = "%s")
  
  # Verify files exist
  expect_true(file.exists(patients_file), "Patient data file should exist")
  expect_true(file.exists(cohort_file), "Cohort definition file should exist")
  
  # Test the cohort definition against patient data
  results <- validate_my_cohort(
    patients_file = patients_file,
    cohort_file = cohort_file,
    print_summary = FALSE  # Suppress output during testing
  )
  
  # Expect all tests to pass
  expect_equal(results$failed_tests, 0,
    info = sprintf("Cohort validation failed for %%d out of %%d patients. Check test output for details.", 
                   results$failed_tests, results$total_tests))
  
  # Additional checks
  expect_gt(results$total_tests, 0, "No patients were tested")
  expect_true(all(c("total_tests", "passed_tests", "failed_tests", "test_results") %%in%% names(results)),
              "Results object missing expected fields")
})
', cohort_name, package_name, cohort_name, package_name)
  
  writeLines(content, file.path(project_path, "tests", "testthat", paste0("test_", cohort_name, ".R")))
}

create_template_patients_excel <- function(project_path) {
  # Create a minimal example Excel file
  library(writexl)
  
  # Example patient data
  person <- data.frame(
    person_id = 1:3,
    gender_concept_id = c(8507, 8532, 8507),
    year_of_birth = c(1980, 1975, 1990),
    race_concept_id = c(8527, 8527, 8527),
    ethnicity_concept_id = c(38003564, 38003564, 38003564)
  )
  
  observation_period <- data.frame(
    observation_period_id = 1:3,
    person_id = 1:3,
    observation_period_start_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
    observation_period_end_date = as.Date(c("2023-12-31", "2023-12-31", "2023-12-31")),
    period_type_concept_id = c(44814724, 44814724, 44814724)
  )
  
  # Example explanation
  explanation <- data.frame(
    patient_id = 1:3,
    reason_id = c(NA, 1, NA),
    explanation = c(
      "INCLUDE - Meets all criteria",
      "EXCLUDE - Failed inclusion rule 1", 
      "INCLUDE - Meets all criteria"
    )
  )
  
  # Create Excel file
  excel_data <- list(
    person = person,
    observation_period = observation_period,
    explanation = explanation
  )
  
  write_xlsx(excel_data, file.path(project_path, "inst", "testdata", "patients.xlsx"))
}

create_template_cohort_json <- function(project_path, cohort_name) {
  # Create a basic cohort JSON template
  content <- sprintf('{
  "ConceptSets": [],
  "PrimaryCriteria": {
    "CriteriaList": [
      {
        "VisitOccurrence": {}
      }
    ],
    "ObservationWindow": {
      "PriorDays": 0,
      "PostDays": 0
    },
    "PrimaryCriteriaLimit": {
      "Type": "All"
    }
  },
  "QualifiedLimit": {
    "Type": "First"
  },
  "ExpressionLimit": {
    "Type": "All"
  },
  "InclusionRules": [],
  "EndStrategy": {
    "DateOffset": {
      "DateField": "EndDate",
      "Offset": 1
    }
  },
  "CensoringCriteria": [],
  "CollapseSettings": {
    "CollapseType": "ERA",
    "EraPad": 0
  },
  "CensorWindow": {},
  "cdmVersionRange": ">=5.0.0"
}')
  
  writeLines(content, file.path(project_path, "inst", "testdata", paste0(cohort_name, ".json")))
}

create_github_actions_workflow <- function(project_path) {
  content <- 'name: Test Cohort Definition

on:
  push:
    branches: [ main, master ]
  pull_request:
    branches: [ main, master ]

jobs:
  test-cohort:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up R
      uses: r-lib/actions/setup-r@v2
      with:
        r-version: "4.4.1"
        
    - name: Install system dependencies
      run: |
        sudo apt-get update
        sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev
    
    - name: Install R dependencies
      run: |
        install.packages(c("remotes", "devtools", "testthat"))
        remotes::install_github("your-org/cohortTdd")  # Update with actual repo
        if (file.exists("DESCRIPTION")) {
          remotes::install_deps(dependencies = TRUE)
        }
      shell: Rscript {0}
    
    - name: Run cohort tests
      run: |
        devtools::test()
      shell: Rscript {0}
    
    - name: Upload test results
      uses: actions/upload-artifact@v3
      if: failure()
      with:
        name: test-results
        path: tests/testthat/
'
  
  writeLines(content, file.path(project_path, ".github", "workflows", "test-cohort.yml"))
}