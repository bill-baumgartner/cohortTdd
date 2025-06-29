# cohortTdd 0.1.0

## Initial Release

### New Features

* **Initial implementation** provides a parameterized testing framework that automatically validates cohort logic against expected patient inclusion/exclusion patterns using TestGenerator and CDMConnector
* **Project setup utilities**: `setup_cohort_testing_project()` creates complete testing projects with templates and CI/CD integration
* **Automated test generation**: Zero-configuration testing - just provide Excel + JSON cohort files

### Package Structure

* Uses modern R package conventions with `inst/testdata/` for test data files
* Cross-platform caching support using `tools::R_user_dir()`
* Integration with TestGenerator for synthetic CDM creation
* CDMConnector integration for cohort execution and attrition analysis

### Documentation

* **Vignettes**: Getting started guide and advanced usage patterns
* **Examples**: Complete sample project demonstrating validation workflow
* **Function documentation**: Comprehensive help for all exported functions

### Dependencies

* TestGenerator (>= 0.1.0): Synthetic CDM data generation
* CDMConnector (>= 1.0.0): OMOP CDM operations and cohort execution
* CirceR: ATLAS cohort JSON to SQL conversion
* Standard R data manipulation: dplyr, readxl, writexl

### Testing

* testthat integration with comprehensive test coverage
* Error handling for common failure modes
* Validation of Excel file structure and cohort JSON format