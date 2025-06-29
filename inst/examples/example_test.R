# Example test file for users to copy/modify
# Save this as: tests/testthat/test_my_cohort.R

library(testthat)
library(cohortTdd)

test_that("My cohort validates correctly", {
  # Test your cohort definition against patient data
  results <- validate_my_cohort(
    patients_file = "data/my_patients.xlsx",
    cohort_file = "cohorts/my_cohort.json",
    print_summary = FALSE  # Suppress output during automated testing
  )
  
  # Expect all tests to pass
  expect_equal(results$failed_tests, 0,
    info = sprintf("Cohort validation failed for %d out of %d patients. Check test output for details.", 
                   results$failed_tests, results$total_tests))
  
  # Additional checks
  expect_gt(results$total_tests, 0, "No patients were tested")
  expect_true(all(c("total_tests", "passed_tests", "failed_tests", "test_results") %in% names(results)),
              "Results object missing expected fields")
})

# Optional: Test with summary output for manual debugging
test_that("My cohort validation (with detailed output)", {
  skip_on_ci()  # Skip on CI to avoid cluttering logs
  
  results <- validate_my_cohort(
    patients_file = "data/my_patients.xlsx", 
    cohort_file = "cohorts/my_cohort.json",
    print_summary = TRUE  # Show detailed output for debugging
  )
  
  expect_equal(results$failed_tests, 0)
})