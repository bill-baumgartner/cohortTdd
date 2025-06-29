library(testthat)
library(cohortTdd)

# Integration tests for the cohortTdd package

test_that("validate_my_cohort function handles missing files gracefully", {
  # Test error handling for non-existent files
  expect_error(
    validate_my_cohort(
      patients_file = "nonexistent.xlsx",
      cohort_file = "also_nonexistent.json",
      print_summary = FALSE
    ),
    class = "error"
  )
})

test_that("setup_cohort_testing_project creates working project", {
  temp_dir <- tempdir()
  project_name <- "test_integration_project"
  
  # Clean up if exists
  project_path <- file.path(temp_dir, project_name)
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  # Create project
  created_path <- setup_cohort_testing_project(
    project_name = project_name,
    path = temp_dir,
    cohort_name = "test_cohort",
    github_actions = FALSE
  )
  
  expect_equal(created_path, project_path)
  
  # Test that created files are functional
  patients_file <- file.path(project_path, "inst", "testdata", "patients.xlsx")
  expect_true(file.exists(patients_file))
  
  # Should be able to read the created Excel file
  sheets <- readxl::excel_sheets(patients_file)
  expect_true("explanation" %in% sheets)
  expect_true("person" %in% sheets)
  
  # Cleanup
  unlink(project_path, recursive = TRUE)
})