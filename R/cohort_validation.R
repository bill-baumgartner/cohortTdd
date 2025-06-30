#' Extract person IDs from Excel file
#'
#' @param xlsx_file_path Path to Excel file containing patient data
#' @return Vector of unique person IDs
#' @export
extract_person_ids <- function(xlsx_file_path) {
  checkmate::assert_string(xlsx_file_path)
  checkmate::assert_file_exists(xlsx_file_path)
  person_data <- readxl::read_excel(xlsx_file_path, sheet = "person")
  unique_ids <- person_data %>%
    dplyr::select(person_id) %>%
    dplyr::distinct() %>%
    dplyr::pull(person_id)
  return(unique_ids)
}

#' Read explanation data from Excel file
#'
#' @param xlsx_file_path Path to Excel file containing explanation tab
#' @return Data frame with patient explanations and expected reason IDs
#' @export
read_explanation_data <- function(xlsx_file_path) {
  checkmate::assert_string(xlsx_file_path)
  checkmate::assert_file_exists(xlsx_file_path)
  explanation_data <- readxl::read_excel(xlsx_file_path, sheet = "explanation")
  return(explanation_data)
}

#' Extract exclusion reason from cohort attrition results
#'
#' @param cohort_attrition Attrition table from CDMConnector
#' @return List with exclusion status, reason_id, and reason text
#' @export
extract_exclusion_reason_from_attrition <- function(cohort_attrition) {
  exclusion_row <- cohort_attrition %>% 
    dplyr::filter(number_subjects == 0) %>% 
    dplyr::slice(1)
  
  if (nrow(exclusion_row) > 0) {
    return(list(
      excluded = TRUE,
      reason_id = exclusion_row$reason_id,
      reason = exclusion_row$reason
    ))
  } else {
    return(list(
      excluded = FALSE,
      reason_id = NA,
      reason = "Patient included in cohort"
    ))
  }
}

#' Prepare cohorts for optimized execution (internal function)
#'
#' @param cohort_set Cohort set from CDMConnector::readCohortSet
#' @param cdm CDM object for context
#' @return The cohort_set (for now, future optimization will compile SQL)
#' @keywords internal
compile_cohort_sql <- function(cohort_set, cdm) {
  # SIMPLIFIED APPROACH: For now, just return the cohort_set
  # This function is a placeholder for future SQL compilation optimization
  # The main benefit currently is that we call generateCohortSet once per patient
  # instead of reading cohort files repeatedly
  
  # Future enhancement: Pre-compile JSON to SQL using CirceR here
  # For now, we'll rely on CDMConnector's internal optimization
  
  return(list(
    cohort_set = cohort_set,
    optimized = FALSE  # Flag for future SQL caching implementation
  ))
}

#' Execute cohorts with current optimization level (internal function)
#'
#' @param cdm CDM object to execute cohorts on
#' @param compiled_cohorts Result from compile_cohort_sql
#' @param name Name for the cohort table
#' @return CDM object with cohort table added
#' @keywords internal
execute_compiled_cohorts <- function(cdm, compiled_cohorts, name = "cohort") {
  # CURRENT IMPLEMENTATION: Use standard generateCohortSet
  # This still provides optimization by using shared CDM and pre-read cohort_set
  
  cohort_set <- compiled_cohorts$cohort_set
  
  # Execute cohort generation (this is still the slow step, but now optimized with shared CDM)
  cdm <- CDMConnector::generateCohortSet(cdm, cohort_set, name = name, overwrite = TRUE)
  
  return(cdm)
}

#' Populate shared CDM with single patient data
#'
#' @param shared_cdm Existing CDM object to populate
#' @param xlsx_file_path Path to Excel file with patient data
#' @param patient_id ID of patient to extract
#' @return CDM object populated with single patient data
#' @export
populate_shared_cdm_with_patient <- function(shared_cdm, xlsx_file_path, patient_id) {
  # Clear existing patient data from all relevant tables
  
  # Get all OMOP CDM table names that might contain patient data
  omop_tables <- c("person", "observation_period", "visit_occurrence", "visit_detail", 
                   "condition_occurrence", "drug_exposure", "procedure_occurrence", 
                   "device_exposure", "measurement", "observation", "death", 
                   "note", "note_nlp", "specimen", "fact_relationship")
  
  # Clear tables that exist in the CDM using newer dplyr::compute approach
  for (table_name in omop_tables) {
    if (table_name %in% names(shared_cdm)) {
      # Use dplyr::compute to clear table
      shared_cdm[[table_name]] <- shared_cdm[[table_name]] %>% 
        dplyr::filter(FALSE) %>%
        dplyr::compute(name = table_name, temporary = FALSE)
    }
  }
  
  # Get all sheet names except explanation
  sheet_names <- readxl::excel_sheets(xlsx_file_path)
  sheet_names <- sheet_names[sheet_names != "explanation"]
  
  # Filter data for single patient and insert into CDM
  for (sheet in sheet_names) {
    df <- readxl::read_excel(xlsx_file_path, sheet = sheet)
    if ("person_id" %in% colnames(df)) {
      filtered_df <- df %>% dplyr::filter(person_id == !!patient_id)
      if (nrow(filtered_df) > 0 && sheet %in% names(shared_cdm)) {
        # Use CDMConnector's insertTable or computeQuery to add data
        shared_cdm <- omopgenerics::insertTable(shared_cdm, name = sheet, table = filtered_df, overwrite = TRUE, temporary = FALSE)
      }
    } else {
      # Tables without person_id (like vocabulary tables) - include all data
      if (nrow(df) > 0 && sheet %in% names(shared_cdm)) {
        shared_cdm <- omopgenerics::insertTable(shared_cdm, name = sheet, table = df, overwrite = TRUE, temporary = FALSE)
      }
    }
  }
  
  return(shared_cdm)
}

#' Create single-patient CDM for testing
#'
#' @param xlsx_file_path Path to Excel file with patient data
#' @param patient_id ID of patient to extract
#' @param outputPath Directory for generated test files
#' @param testName Base name for test files
#' @return CDM object for single patient
#' @export
create_single_patient_cdm <- function(xlsx_file_path, patient_id, outputPath, testName) {
  # Ensure output directory exists
  if (!dir.exists(outputPath)) {
    dir.create(outputPath, recursive = TRUE)
  }
  
  # Get all sheet names except explanation
  sheet_names <- readxl::excel_sheets(xlsx_file_path)
  sheet_names <- sheet_names[sheet_names != "explanation"]
  
  # Filter data for single patient
  filtered_sheets <- list()
  for (sheet in sheet_names) {
    df <- readxl::read_excel(xlsx_file_path, sheet = sheet)
    if ("person_id" %in% colnames(df)) {
      filtered_df <- df %>% dplyr::filter(person_id == !!patient_id)
      filtered_sheets[[sheet]] <- filtered_df
    } else {
      filtered_sheets[[sheet]] <- df
    }
  }
  
  # Create temporary Excel file for this patient
  temp_file <- tempfile(pattern = paste0("patient_", patient_id, "_"), fileext = ".xlsx")
  writexl::write_xlsx(filtered_sheets, path = temp_file)
  
  # Generate test case JSON
  TestGenerator::readPatients.xl(
    filePath = temp_file,
    testName = paste0(testName, "_patient_", patient_id),
    outputPath = outputPath,
    cdmVersion = "5.4"
  )
  
  # Create CDM
  cdm <- TestGenerator::patientsCDM(
    pathJson = outputPath,
    testName = paste0(testName, "_patient_", patient_id),
    cdmVersion = "5.4"
  )
  
  # Clean up temp file
  unlink(temp_file)
  
  return(cdm)
}

#' Validate a single cohort definition against patient test data
#'
#' @param patients_file Path to Excel file with patient data and explanations
#' @param cohort_file Path to single cohort JSON file (alternative to cohorts_path)
#' @param cohorts_path Directory containing cohort definition JSON files (alternative to cohort_file)
#' @param print_summary Whether to print detailed summary (default TRUE)
#' @return List with validation results and summary statistics
#' @export
#' @examples
#' \dontrun{
#' # Quick validation with single cohort file
#' results <- validate_my_cohort(
#'   patients_file = "my_patients.xlsx",
#'   cohort_file = "my_cohort.json"
#' )
#' 
#' # Validation with directory of cohorts
#' results <- validate_my_cohort(
#'   patients_file = "my_patients.xlsx", 
#'   cohorts_path = "cohorts/"
#' )
#' }
validate_my_cohort <- function(patients_file, cohort_file = NULL, cohorts_path = NULL, print_summary = TRUE, verbose = TRUE) {
  # Input validation - check logical conditions first
  checkmate::assert_string(patients_file)
  checkmate::assert_string(cohort_file, null.ok = TRUE)
  checkmate::assert_string(cohorts_path, null.ok = TRUE)
  checkmate::assert_logical(print_summary, len = 1)
  checkmate::assert_logical(verbose, len = 1)
  
  # Check cohort file/path logic before file existence
  if (is.null(cohort_file) && is.null(cohorts_path)) {
    stop("Either cohort_file or cohorts_path must be provided")
  }
  
  if (!is.null(cohort_file) && !is.null(cohorts_path)) {
    stop("Provide either cohort_file OR cohorts_path, not both")
  }
  
  # Check file existence after logical validation
  if (!file.exists(patients_file)) {
    stop("patients_file does not exist: ", patients_file)
  }
  # Set up TestGenerator data cache (cross-platform)
  data_dir <- tools::R_user_dir("TestGenerator", which = "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  Sys.setenv(STUDY_DATASETS = data_dir)
  
  # Input validation
  if (is.null(cohort_file) && is.null(cohorts_path)) {
    stop("Either cohort_file or cohorts_path must be provided")
  }
  
  if (!is.null(cohort_file) && !is.null(cohorts_path)) {
    stop("Provide either cohort_file OR cohorts_path, not both")
  }
  
  if (!file.exists(patients_file)) {
    stop("patients_file does not exist: ", patients_file)
  }
  
  # Inform user about validation process
  if (print_summary || verbose) {
    explanation_data <- read_explanation_data(patients_file)
    expected_patient_count <- nrow(explanation_data)
    
    cat("\n** Running cohort validation for", expected_patient_count, "patients...\n")
    cat("   - Initial setup: ~90 seconds (downloads empty CDM data - ~1.2G disk space required)\n")
    cat("   - Per-patient testing: ~5-10 seconds each (cohort generation overhead) - further optimization is needed\n")
    cat("   - To speed up future runs: Set EUNOMIA_DATA_FOLDER in your .Renviron file\n\n")
    cat("   Validation summary will appear when complete...\n\n")
  }
  
  # Handle single cohort file by creating temporary directory
  if (!is.null(cohort_file)) {
    if (!file.exists(cohort_file)) {
      stop("cohort_file does not exist: ", cohort_file)
    }
    
    # Create temporary directory and copy cohort file
    temp_cohorts_dir <- tempdir()
    cohort_dest <- file.path(temp_cohorts_dir, basename(cohort_file))
    
    # Only copy if source and destination are different
    if (normalizePath(cohort_file) != normalizePath(cohort_dest, mustWork = FALSE)) {
      file.copy(cohort_file, cohort_dest, overwrite = TRUE)
    }
    cohorts_path <- temp_cohorts_dir
  } else if (!dir.exists(cohorts_path)) {
    stop("cohorts_path directory does not exist: ", cohorts_path)
  }
  
  # Run validation
  results <- validate_cohort_definitions(patients_file, cohorts_path, verbose = verbose)
  
  # Print summary if requested
  if (print_summary) {
    print_validation_summary(results)
  }
  
  return(results)
}

#' Validate cohort definitions against patient test data (internal function)
#'
#' @param patients_file Path to Excel file with patient data and explanations
#' @param cohorts_path Directory containing cohort definition JSON files
#' @return List with validation results and summary statistics
#' @export
validate_cohort_definitions <- function(patients_file, cohorts_path, verbose = TRUE) {
  # Input validation
  checkmate::assert_string(patients_file)
  checkmate::assert_file_exists(patients_file)
  checkmate::assert_string(cohorts_path)
  checkmate::assert_directory_exists(cohorts_path)
  checkmate::assert_logical(verbose, len = 1)
  # Read explanation data to get all patients and their expected outcomes
  explanation_data <- read_explanation_data(patients_file)
  
  # Initialize results tracking
  test_results <- list()
  total_tests <- 0
  passed_tests <- 0
  failed_tests <- 0
  
  # OPTIMIZATION: Create shared CDM and read cohort set once
  
  # Create shared empty CDM once
  shared_cdm <- tryCatch({
    TestGenerator::getEmptyCDM(cdmName = "shared_test_cdm", cdmVersion = "5.4")
  }, error = function(e) {
    stop("Failed to create shared CDM: ", e$message)
  })
  
  # Read cohort set once
  cohort_set <- tryCatch({
    CDMConnector::readCohortSet(cohorts_path)
  }, error = function(e) {
    stop("Failed to read cohort set: ", e$message)
  })
  
  # OPTIMIZATION: Pre-compile cohort SQL once
  compiled_cohorts <- tryCatch({
    compile_cohort_sql(cohort_set, shared_cdm)
  }, error = function(e) {
    stop("Failed to compile cohort SQL: ", e$message)
  })
  
  # Test each patient
  for (i in 1:nrow(explanation_data)) {
    patient_id <- explanation_data$patient_id[i]
    expected_excluded <- !is.na(explanation_data$reason_id[i])
    expected_reason_id <- explanation_data$reason_id[i]
    explanation_text <- explanation_data$explanation[i]
    
    total_tests <- total_tests + 1
    
    # Test this patient with better error handling
    
    outputPath <- file.path(tempdir(), "testCases")
    testName <- "cohort_validation_test"
    
    # OPTIMIZATION: Populate shared CDM with patient data instead of creating new CDM
    cdm_result <- tryCatch({
      populate_shared_cdm_with_patient(shared_cdm, patients_file, patient_id)
    }, error = function(e) {
      list(error = paste("CDM population failed:", e$message))
    })
    
    if ("error" %in% names(cdm_result)) {
      failed_tests <- failed_tests + 1
      test_results[[patient_id]] <- list(
        patient_id = patient_id,
        status = "ERROR",
        explanation = explanation_text,
        error = cdm_result$error
      )
      next
    }
    
    cohort_result <- tryCatch({
      # OPTIMIZATION: Use pre-compiled SQL instead of full generateCohortSet
      cdm <- execute_compiled_cohorts(cdm_result, compiled_cohorts, name = "test_cohorts")
      
      attrition_result <- CDMConnector::attrition(cdm[["test_cohorts"]])
      
      attrition_result
    }, error = function(e) {
      list(error = paste("Cohort generation failed:", e$message))
    })
    
    if ("error" %in% names(cohort_result)) {
      failed_tests <- failed_tests + 1
      test_results[[patient_id]] <- list(
        patient_id = patient_id,
        status = "ERROR", 
        explanation = explanation_text,
        error = cohort_result$error
      )
      next
    }
    
    # Extract results and validate
    actual_result <- extract_exclusion_reason_from_attrition(cohort_result)
    
    # Validate results
    validation_passed <- TRUE
    error_message <- ""
    
    if (expected_excluded && !actual_result$excluded) {
      validation_passed <- FALSE
      error_message <- sprintf("Expected patient %d to be excluded, but was included", patient_id)
    } else if (!expected_excluded && actual_result$excluded) {
      validation_passed <- FALSE
      error_message <- sprintf("Expected patient %d to be included, but was excluded with reason %s", 
                              patient_id, actual_result$reason_id)
    } else if (expected_excluded && actual_result$excluded) {
      if (!is.na(expected_reason_id) && actual_result$reason_id != expected_reason_id) {
        validation_passed <- FALSE
        error_message <- sprintf("Patient %d excluded with wrong reason: expected %s, got %s", 
                                patient_id, expected_reason_id, actual_result$reason_id)
      }
    }
    
    # Record result
    if (validation_passed) {
      passed_tests <- passed_tests + 1
      test_results[[patient_id]] <- list(
        patient_id = patient_id,
        status = "PASSED",
        explanation = explanation_text,
        expected_excluded = expected_excluded,
        actual_excluded = actual_result$excluded,
        expected_reason_id = expected_reason_id,
        actual_reason_id = actual_result$reason_id
      )
      if (verbose) cat(sprintf("Test of Patient %d: PASSED\n", patient_id), file = stderr())
    } else {
      failed_tests <- failed_tests + 1
      test_results[[patient_id]] <- list(
        patient_id = patient_id,
        status = "FAILED",
        explanation = explanation_text,
        expected_excluded = expected_excluded,
        actual_excluded = actual_result$excluded,
        expected_reason_id = expected_reason_id,
        actual_reason_id = actual_result$reason_id,
        error = error_message
      )
      if (verbose) cat(sprintf("Test of Patient %d: FAILED - %s\n", patient_id, error_message), file = stderr())
    }
  }
  
  # Return results summary
  results_summary <- list(
    total_tests = total_tests,
    passed_tests = passed_tests,
    failed_tests = failed_tests,
    test_results = test_results
  )
  
  return(results_summary)
}

#' Print validation summary to console
#'
#' @param results_summary Results from validate_cohort_definitions
#' @export
print_validation_summary <- function(results_summary) {
  total_tests <- results_summary$total_tests
  passed_tests <- results_summary$passed_tests
  failed_tests <- results_summary$failed_tests
  test_results <- results_summary$test_results
  
  # Print comprehensive summary
  cat("\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("                       COHORT VALIDATION SUMMARY\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat(sprintf("Total patients tested: %d\n", total_tests))
  cat(sprintf("Tests passed: %d\n", passed_tests))
  cat(sprintf("Tests failed: %d\n", failed_tests))
  cat(sprintf("Success rate: %.1f%%\n", if(total_tests > 0) (passed_tests/total_tests)*100 else 0))
  
  # Show failed tests
  if (failed_tests > 0) {
    cat("\n** FAILED TESTS:\n")
    cat(paste(rep("-", 50), collapse=""), "\n")
    for (result in test_results) {
      if (result$status %in% c("FAILED", "ERROR")) {
        cat(sprintf("Patient %d: %s\n", result$patient_id, result$explanation))
        if (!is.null(result$error)) {
          cat(sprintf("  Error: %s\n", result$error))
        }
        cat("\n")
      }
    }
  }
  
  # Show detailed results
  cat("\n** DETAILED RESULTS:\n")
  cat(paste(rep("-", 50), collapse=""), "\n")
  for (result in test_results) {
    status_icon <- switch(result$status,
                         "PASSED" = "[PASS]",
                         "FAILED" = "[FAIL]", 
                         "ERROR" = "[ERROR]")
    cat(sprintf("%s Patient %d: %s\n", status_icon, result$patient_id, result$explanation))
  }
  cat("\n")
}