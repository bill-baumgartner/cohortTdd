library(testthat)

# Simple tests that don't require external dependencies
test_that("print_validation_summary handles empty results", {
  empty_results <- list(
    total_tests = 0,
    passed_tests = 0,
    failed_tests = 0,
    test_results = list()
  )
  
  # Should not error
  expect_no_error(
    capture.output(print_validation_summary(empty_results))
  )
  
  output <- capture.output(print_validation_summary(empty_results))
  expect_true(any(grepl("Total patients tested: 0", output)))
})

test_that("extract_exclusion_reason_from_attrition handles edge cases", {
  # Empty data frame
  empty_attrition <- data.frame(
    reason_id = integer(0),
    reason = character(0),
    number_subjects = integer(0)
  )
  
  result <- extract_exclusion_reason_from_attrition(empty_attrition)
  expect_false(result$excluded)
  expect_true(is.na(result$reason_id))
  
  # All patients included
  included_attrition <- data.frame(
    reason_id = c(1, 2),
    reason = c("Initial", "Second"),
    number_subjects = c(10, 5)
  )
  
  result2 <- extract_exclusion_reason_from_attrition(included_attrition)
  expect_false(result2$excluded)
  expect_equal(result2$reason, "Patient included in cohort")
  
  # Patient excluded at first step
  excluded_attrition <- data.frame(
    reason_id = c(1, 2),
    reason = c("Initial", "Excluded here"),
    number_subjects = c(0, 0)
  )
  
  result3 <- extract_exclusion_reason_from_attrition(excluded_attrition)
  expect_true(result3$excluded)
  expect_equal(result3$reason_id, 1)
  expect_equal(result3$reason, "Initial")
})

test_that("validate_my_cohort input validation works", {
  # Test that it properly validates inputs without calling the actual validation
  expect_error(
    validate_my_cohort("test.xlsx"),
    "Either cohort_file or cohorts_path must be provided"
  )
  
  expect_error(
    validate_my_cohort("test.xlsx", cohort_file = "test.json", cohorts_path = "test_dir"),
    "Provide either cohort_file OR cohorts_path, not both"
  )
  
  expect_error(
    validate_my_cohort("nonexistent.xlsx", cohort_file = "test.json"),
    "patients_file does not exist"
  )
})