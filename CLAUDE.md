# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the cohortTdd R package for test-driven development of OHDSI cohort definitions using TestGenerator and CDMConnector packages. The package validates cohort logic by running it against synthetic patient data and checking which patients are included/excluded.

## Key Dependencies

- TestGenerator: Creates synthetic CDM data from Excel patient files
- CDMConnector: Manages cohort definitions and executes cohort SQL
- CirceR: Converts ATLAS cohort JSON to SQL
- testthat: Unit testing framework

## Common Commands

### Running Tests
```r
# Run all tests
testthat::test_check("hctCohortTest")

# Run specific test file
testthat::test_file("tests/testthat/test_hct_cohort.R")
```

### Package Development
```r
# Load package in development
devtools::load_all()

# Install package
devtools::install()

# Check package
devtools::check()
```

## Architecture

### Core Workflow
1. **Patient Data Processing**: Excel files with multiple sheets (person, condition_occurrence, drug_exposure, measurement) are filtered by person_id to create individual patient test cases
2. **CDM Generation**: TestGenerator converts Excel data to synthetic CDM format
3. **Cohort Testing**: CDMConnector executes cohort definitions against synthetic data
4. **Validation**: Results are compared against expected inclusion/exclusion patterns

### Key Functions in R/temp.R
- `extract_person_ids()`: Extracts unique person IDs from Excel patient data
- `filter_excel_by_person_ids()`: Creates separate Excel files for each patient
- `create_temp_patient_xlsx_files()`: Alternative approach for patient file creation

### Test Data Structure
- `inst/testdata/hct_patients.xlsx`: Multi-sheet Excel with synthetic patient data
- `inst/testdata/hct_cohort.json`: ATLAS cohort definition in JSON format
- `inst/testdata/hct_cohort_explanations.txt`: Documents expected inclusion/exclusion for each patient
- `testthat/testCases/`: Generated JSON test cases for TestGenerator

### Testing Pattern
The package uses a fully parameterized testing approach that requires no code changes when adding new patients or cohort definitions:

1. **Automatic Patient Discovery**: Tests automatically read all patients from the "explanation" tab in the Excel file
2. **Dynamic Test Generation**: Each patient gets validated automatically based on their expected inclusion/exclusion reason
3. **Cohort Agnostic**: Works with any cohort definition JSON files in the testdata directory
4. **Zero Configuration**: Just provide Excel file + cohort JSON, no additional test code needed

### Test Execution Flow
1. Load Excel patient data and explanation tab
2. For each patient: create single-patient CDM using TestGenerator
3. Execute cohort definition using CDMConnector
4. Extract exclusion reason from attrition table (where number_subjects = 0)
5. Compare actual vs expected results and generate comprehensive summary

### Patient Validation Logic
Expected results are defined in the "explanation" tab:
- **Included patients** (reason_id = NA): Patients 1, 6, 7
- **Excluded patients** with specific reason_ids:
  - Patient 2: reason_id = 1 (testosterone exposures <7 days apart)
  - Patient 3: reason_id = 2 (estradiol exposure)  
  - Patient 4: reason_id = 3 (no HCT before index date)
  - Patient 5: reason_id = 4 (<4 HCT after index date)

### Adding New Test Cases
To test new patients or cohorts:
1. Add patients to Excel file with appropriate explanation tab entries
2. Place cohort JSON files in inst/testdata/ 
3. Run tests - no code changes required