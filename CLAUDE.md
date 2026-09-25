# teal.modules.gtsummary Development Guide

## Package Overview

`teal.reporter` is part of the `teal` framework and provides with an API and shiny modules to manage the reporter functionality on a `teal` app.
A report is a set of markup language, code chunks and respective outputs (tables, listings and/or graphs) that can be downloaded
by the user in several formats.
It should also contain data to restore the report on a `teal` application.

It provides with 4 main features to the framework:

- Shiny modules with UI and server functions to manage reports on a `teal` session
- `Reporter` object that manages multiple reports on a teal session
- `card` object that represent the report of a module in a teal application
- `report` object that extends on `teal_data` API by adding API to maintain a representation of the report, in addition to all features `teal_data` and `qenv` already provide.

## Development Context

The main feature of this package is to provide `teal` apps with reporting capabilities, however it is also intended to allow for extensions, allowing for more data types to be supported as well as custom rendering of the report.

### Relationships with other packages

Direct dependencies:

- `teal.data`: `teal_reporter` object extends the `teal_data` object by adding new functionalities
  - Any issue with `join_keys` should be addressed in this package.
- `teal.code`: `teal_data` extends a `qenv` object from `teal.code`, where the code execution and reproducibily features are implemented.
  - Any issue with code execution and reproducibility should be addressed in this package
  - The only exception is the the `card` object management, which automatically tracks code and outputs to be used in the report

Usage in other framework packages:

- `teal`: uses the API in `teal.reporter` to maintain an instance of the reporter and uses the exported shiny modules for the interface
  - Converts the `data` argument in `teal::init()` function to the `teal_reporter` object that is used in the modules
- teal module: The `data` argument passed on to modules uses the `teal_reporter` data type.
  - Automatically tracks the code execution and output objects
  - It is used in custom modules as well as R packages on CRAN: `teal.modules.clinical` and `teal.modules.general`

*Note*: output object are captured from the code execution `teal_reporter() |> within(plot(1:10))` will capture a plot.
It is the equivalent to what is being printed on the console when executing code.

### Extendability

`teal.reporter` should allow for its functionalities to be extended by users according to their specific needs.
This can be achieved by:

- Extending the `Reporter` class
- Using the `Reporter$set_template()` method that processes each card that is being added
- Adding support for new data types or overwriting existing defaults

The latter framework is implemented on the functions that print the cards as well as the `teal_card` object.
This uses a 2-layer dispatch mechanism that allows functions `to_rmd()` and `toHTML()` to include new data types and overwrite existing ones.
This is the reason for the existence of `.to_rmd()` and `.toHTML()` that implement internal methods that serve as defaults for the supported data types.

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

```text
package_name/
├── .gitlab-ci.yml    # CI/CD workflows
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── CLAUDE.md         # Development guide for AI agents (this file)
├── DESCRIPTION       # Package metadata
├── NAMESPACE         # Exports and imports automa
├── NEWS.md           # Change log
├── README.md         # Package overview
├── _pkgdown.yml      # Documentation website config
├── .lintr            # Linting configuration
└── .Rbuildignore     # Build exclusions
```

### Naming Conventions

- **Function names**: Use `snake_case` consistently
- **Class names**: Use `PascalCase` (e.g., `TealAppDriver`)
- **Module functions**: Prefix UI functions with `ui_` and server functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`, `validations.R`)
- **Module files**: Use pattern `tm_<name>.R` for teal modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run `pre-commit` hooks**: Always run `pre-commit run --all-files` before committing. Fix any issues it reports - the error messages are informative and will guide you. It automatically checks code style, documentation, linting, and other quality issues.
- **Follow `tidyverse` style**: General R code style follows the `tidyverse` style guide.
- **Documentation**: All exported functions must have `roxygen2` documentation. Run `devtools::document()` to update documentation.
- **Formatting** rules are configured in the `.lintr` file.

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide significant value
- **Version constraints**: Specify minimum versions for critical dependencies
- **Ecosystem coherence**: Prefer packages already used within teal ecosystem

### Import Best Practices

Avoid importing package functions via roxygen2 (`#' @import pkg`)tags in favor of explicit namespacing for clarity when appropriate.
When needed prefer specific imports over full package imports.

### Code Style for Modules

- **Use `tidyverse` style**: Write clear, readable code using `dplyr`, `ggplot2` patterns
- **Use `magrittr` pipes in reproducible execution**: For code executed for `teal_data`/`qenv` data objects with `eval_code()` and `within()`
- **Use crane and gtsummary**: For statistical tables and summaries
- **Error handling**: Implement proper validation using `checkmate` and `shiny::validate(teal::need_input(...))`

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested through public interfaces
- **Precise, focused tests**: Each test should verify one specific behavior
- **High coverage**: Maintain at least 80% test coverage as measured by `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add `testthat::skip_if_not_installed(package_name)` only for dependencies in SUGGESTS or related to tests cases

### Shiny Module Testing

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks, error generation, snapshots, regexp search). Test UI scenarios and interactions with `teal::TealAppDriver` (based on `shinytest2::AppDriver`) for integration testing
- **Reactive behavior**: Test reactive chains and side effects

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for `module_example.R`
- **Logical grouping**: Group related tests using `describe()` when beneficial
- **Test data**: Create minimal test datasets, avoid external dependencies

## Documentation and Communication

### Package Documentation

- **`README.md`**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have `roxygen2` documentation
- **`NEWS.md`**: Detailed changelog following semantic versioning

### Package Version Management

Do not change versions on your own.
There is a CI/CD workflow that manages the versions automatically on the `main` branch.

## CI/CD and Development Workflow

### Gitlab Workflows

`.gitlab-ci.yml` reuses CI/CD tasks, such as running all unit tests, `R CMD check`, code quality checks, style checks and website generation.

### GitHub Workflows

Use r.pkg.template workflows for consistency:

- `check.yaml`: R CMD check, unit tests, coverage
- `docs.yaml`: Documentation building and deployment
- `audit.yaml`: Security and dependency auditing
- `pkgdown.yaml`: Website generation

## Quality Assurance

### Code Quality Metrics

- **Test Coverage**: ≥80% line coverage
- **Linting**: No lint violations using configured `.lintr`
- **Documentation**: 100% of exports documented
- **Dependencies**: Minimal and justified dependencies only

### Code Review Process

- **Pull Request Reviews**: All changes require review
- **Automated Checks**: CI must pass before merging
- **Breaking Changes**: Require special consideration and communication
- **Documentation Updates**: Must accompany functional changes

### Performance Considerations

- **Shiny Reactivity**: Minimize unnecessary reactive computations
- **Data Processing**: Use efficient data manipulation patterns
- **Memory Usage**: Consider memory implications for large datasets
- **Loading Time**: Optimize package loading and module initialization

## Maintenance Guidelines

- **Long-term Support**: Maintain backward compatibility when possible
- **Deprecation**: Use `lifecycle` package for function deprecation
