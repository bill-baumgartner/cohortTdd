library(testthat)
library(cohortTdd)

# Test setup_cohort_testing_project function
test_that("setup_cohort_testing_project creates correct directory structure", {
  temp_parent_dir <- tempdir()
  project_name <- "test_cohort_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  # Ensure project doesn't already exist
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  # Create project
  result_path <- setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "test_cohort",
    author_name = "Test Author",
    github_actions = TRUE
  )
  
  # Test return value
  expect_equal(result_path, project_path)
  
  # Test directory structure
  expect_true(dir.exists(project_path))
  expect_true(dir.exists(file.path(project_path, "inst", "testdata")))
  expect_true(dir.exists(file.path(project_path, "tests", "testthat")))
  expect_true(dir.exists(file.path(project_path, ".github", "workflows")))
  
  # Test required files exist
  expect_true(file.exists(file.path(project_path, "DESCRIPTION")))
  expect_true(file.exists(file.path(project_path, "README.md")))
  expect_true(file.exists(file.path(project_path, "inst", "testdata", "patients.xlsx")))
  expect_true(file.exists(file.path(project_path, "inst", "testdata", "test_cohort.json")))
  expect_true(file.exists(file.path(project_path, "tests", "testthat", "test_test_cohort.R")))
  expect_true(file.exists(file.path(project_path, ".github", "workflows", "test-cohort.yml")))
  
  # Cleanup
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project without GitHub Actions", {
  temp_parent_dir <- tempdir()
  project_name <- "test_cohort_no_gh"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "test_cohort",
    author_name = "Test Author", 
    github_actions = FALSE
  )
  
  # Should not create .github directory
  expect_false(dir.exists(file.path(project_path, ".github")))
  
  # But should still create other directories
  expect_true(dir.exists(file.path(project_path, "inst", "testdata")))
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project fails if directory exists", {
  temp_parent_dir <- tempdir()
  project_name <- "existing_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  # Create the directory first
  dir.create(project_path, showWarnings = FALSE)
  
  expect_error(
    setup_cohort_testing_project(
      project_name = project_name,
      path = temp_parent_dir
    ),
    "Project directory already exists"
  )
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project creates valid DESCRIPTION file", {
  temp_parent_dir <- tempdir()
  project_name <- "test.desc.project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "my_cohort",
    author_name = "Jane Doe"
  )
  
  desc_file <- file.path(project_path, "DESCRIPTION")
  desc_content <- readLines(desc_file)
  
  expect_true(any(grepl("Package: test.desc.project", desc_content)))
  expect_true(any(grepl("Jane Doe", desc_content)))
  expect_true(any(grepl("cohortTdd", desc_content)))
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project creates valid README", {
  temp_parent_dir <- tempdir()
  project_name <- "test_readme_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "diabetes_cohort",
    author_name = "Test Author"
  )
  
  readme_file <- file.path(project_path, "README.md")
  readme_content <- readLines(readme_file)
  
  expect_true(any(grepl("# test_readme_project", readme_content)))
  expect_true(any(grepl("diabetes_cohort", readme_content)))
  expect_true(any(grepl("library\\(cohortTdd\\)", readme_content)))
  expect_true(any(grepl("validate_my_cohort", readme_content)))
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project creates valid test file", {
  temp_parent_dir <- tempdir()
  project_name <- "test_testfile_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "heart_failure",
    author_name = "Test Author"
  )
  
  test_file <- file.path(project_path, "tests", "testthat", "test_heart_failure.R")
  test_content <- readLines(test_file)
  
  expect_true(any(grepl("library\\(cohortTdd\\)", test_content)))
  expect_true(any(grepl("heart_failure cohort validates correctly", test_content)))
  expect_true(any(grepl("validate_my_cohort", test_content)))
  expect_true(any(grepl("heart_failure.json", test_content)))
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project creates valid Excel file", {
  temp_parent_dir <- tempdir()
  project_name <- "test_excel_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir
  )
  
  excel_file <- file.path(project_path, "inst", "testdata", "patients.xlsx")
  
  # Test that Excel file can be read
  sheets <- readxl::excel_sheets(excel_file)
  expect_true("person" %in% sheets)
  expect_true("observation_period" %in% sheets)
  expect_true("explanation" %in% sheets)
  
  # Test explanation sheet structure
  explanation_data <- readxl::read_excel(excel_file, sheet = "explanation")
  expect_true(all(c("patient_id", "reason_id", "explanation") %in% colnames(explanation_data)))
  expect_equal(nrow(explanation_data), 3)
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project creates valid JSON file", {
  temp_parent_dir <- tempdir()
  project_name <- "test_json_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "test_cohort"
  )
  
  json_file <- file.path(project_path, "inst", "testdata", "test_cohort.json")
  json_content <- readLines(json_file, warn = FALSE)
  json_text <- paste(json_content, collapse = "")
  
  # Test that it's valid JSON
  expect_true(jsonlite::validate(json_text))
  
  # Test that it contains expected cohort structure
  cohort_data <- jsonlite::fromJSON(json_text)
  expect_true("ConceptSets" %in% names(cohort_data))
  expect_true("PrimaryCriteria" %in% names(cohort_data))
  expect_true("InclusionRules" %in% names(cohort_data))
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project creates valid GitHub Actions workflow", {
  temp_parent_dir <- tempdir()
  project_name <- "test_gh_project"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    github_actions = TRUE
  )
  
  workflow_file <- file.path(project_path, ".github", "workflows", "test-cohort.yml")
  workflow_content <- readLines(workflow_file)
  
  expect_true(any(grepl("name: Test Cohort Definition", workflow_content)))
  expect_true(any(grepl("r-version: \"4.4.1\"", workflow_content)))
  expect_true(any(grepl("cohortTdd", workflow_content)))
  expect_true(any(grepl("devtools::test", workflow_content)))
  
  unlink(project_path, recursive = TRUE)
})

test_that("setup_cohort_testing_project handles special characters in names", {
  temp_parent_dir <- tempdir()
  project_name <- "test-special-chars"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir,
    cohort_name = "diabetes_type_2",
    author_name = "Dr. Jane Smith-Doe"
  )
  
  # Should handle hyphens in project name for DESCRIPTION package name
  desc_content <- readLines(file.path(project_path, "DESCRIPTION"))
  expect_true(any(grepl("Package: test.special.chars", desc_content)))
  expect_true(any(grepl("Dr. Jane Smith-Doe", desc_content)))
  
  unlink(project_path, recursive = TRUE)
})

# Test default parameter behavior
test_that("setup_cohort_testing_project uses correct defaults", {
  temp_parent_dir <- tempdir()
  project_name <- "test_defaults"
  project_path <- file.path(temp_parent_dir, project_name)
  
  if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
  
  # Call with minimal parameters
  setup_cohort_testing_project(
    project_name = project_name,
    path = temp_parent_dir
  )
  
  # Check that defaults were used
  readme_content <- readLines(file.path(project_path, "README.md"))
  expect_true(any(grepl("my_cohort", readme_content)))
  
  desc_content <- readLines(file.path(project_path, "DESCRIPTION"))
  expect_true(any(grepl("Your Name", desc_content)))
  
  # GitHub Actions should be enabled by default
  expect_true(dir.exists(file.path(project_path, ".github")))
  
  unlink(project_path, recursive = TRUE)
})

# Test error handling
test_that("setup_cohort_testing_project validates inputs", {
  # Test with empty project name
  expect_error(
    setup_cohort_testing_project(""),
    "project_name cannot be empty"
  )
  
  # Test with invalid path
  expect_error(
    setup_cohort_testing_project("test", path = "/nonexistent/path"),
    "Parent directory does not exist"
  )
})

# Cleanup after all tests
teardown({
  # Clean up any remaining test directories
  temp_dirs <- list.dirs(tempdir(), full.names = TRUE, recursive = FALSE)
  test_dirs <- temp_dirs[grepl("test.*project", basename(temp_dirs))]
  for (dir in test_dirs) {
    if (dir.exists(dir)) {
      unlink(dir, recursive = TRUE)
    }
  }
})