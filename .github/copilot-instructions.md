# Copilot Instructions for lfl

## Project Overview

`lfl` is an R package providing various algorithms related to linguistic fuzzy logic: mining for linguistic fuzzy association rules, composition of fuzzy relations, performing perception-based logical deduction (PbLD), and forecasting time-series using fuzzy rule-based ensemble (FRBE). The package also contains basic fuzzy-related algebraic functions capable of handling missing values in different styles (Bochvar, Sobocinski, Kleene etc.), computation of Sugeno integrals and fuzzy transform.

## Repository Structure

- `R/` - R source code with roxygen2 documentation
- `src/` - C++ source code using Rcpp (if any)
- `tests/testthat/` - Unit tests using testthat framework (if any)
- `man/` - Generated documentation (auto-generated, do not edit manually)
- `vignettes/` - Package vignettes
- `temp/vignette-src/` - Original LaTeX vignette source (for reference, do not modify)
- `.github/workflows/` - CI/CD workflows for R CMD check, pkgdown

## Development Setup

### Prerequisites
- R >= 3.6
- C++ compiler (for Rcpp components)
- Required R packages: Rcpp, devtools, roxygen2, testthat, knitr, rmarkdown

### Building and Testing
```r
# Install dependencies
devtools::install_deps(dependencies = TRUE)

# Build documentation
devtools::document()

# Run R CMD check
devtools::check()

# Build package
devtools::build()

# Build pkgdown site
pkgdown::build_site()
```

## Coding Standards

### R Code

1. **Documentation**: All exported functions must have complete roxygen2 documentation including:
   - `@title` and description
   - `@param` for all parameters
   - `@return` for return values
   - `@seealso` for references to related functions
   - `@examples` with working examples
   - `@export` for exported functions

2. **Naming Conventions**:
   - Functions: lowercase with dots (e.g., `algebra()`, `is.algebra()`)
   - Internal functions: prefix with `.` (e.g., `.mustBeNumericScalar()`)
   - Variables: camelCase or lowercase
   - Use consistent naming patterns within the package

3. **Error Handling**:
   - Use internal validation functions (e.g., `.mustBe()`, `.mustBeNumericScalar()`)
   - Provide clear error messages
   - Validate input parameters at function entry

4. **Style**:
   - Use consistent indentation (4 spaces)
   - Use `<-` for assignment
   - Follow existing code style in the package
   - Keep functions focused and modular

### C++ Code (if applicable)

1. **Standards**:
   - Use Rcpp for R/C++ interface
   - Follow C++ best practices
   - Add comments for complex algorithms

2. **Documentation**:
   - Use Rcpp attributes for exported functions: `// [[Rcpp::export]]`
   - Add roxygen2 documentation above exported functions
   - Internal functions should have clear comments

## Dependencies

### R Package Dependencies
- Runtime: Rcpp, foreach, forecast, plyr, tseries, e1071, tibble
- Build/Link: Rcpp
- Suggests: testthat, doMC, knitr, rmarkdown, R.rsp

### Adding Dependencies
- Add to appropriate field in DESCRIPTION (Imports, Suggests, LinkingTo)
- Document why the dependency is needed
- Keep dependencies minimal

## Documentation

- Use roxygen2 for all documentation
- Run `devtools::document()` to regenerate man/*.Rd files
- README.md is generated from README.Rmd - edit the .Rmd file only
- Vignettes in `vignettes/*.Rmd`
- Package website built with pkgdown at https://beerda.github.io/lfl/

## Package Structure

### Main Components

1. **Fuzzy Algebras** (`algebra.R`, `algebraNA.R`):
   - T-norms (Gödel, Goguen, Łukasiewicz)
   - T-conorms, residua, bi-residua, negations
   - Extensions for handling missing values

2. **Fuzzy Sets** (`fsets.R`, `fcut.R`, `lcut.R`):
   - Creation and manipulation of fuzzy sets
   - Fuzzy partitioning of data
   - Linguistic expressions

3. **Linguistic Expressions** (`ctx.R`, `lingexpr.R`, `hedge.R`):
   - Context definitions
   - Evaluative linguistic expressions
   - Linguistic hedges

4. **Fuzzy Relations** (`compose.R`):
   - Composition of fuzzy relations
   - Various composition types

5. **Association Rules** (`searchrules.R`, `farules.R`):
   - Mining linguistic fuzzy association rules
   - Rule manipulation and filtering

6. **Perception-based Logical Deduction** (`pbld.R`, `fire.R`, `perceive.R`):
   - PbLD inference method
   - Rule firing and perception

7. **Fuzzy Rule-Based Ensemble** (`frbe.R`, `frbemodel.R`):
   - Time series forecasting
   - Ensemble methods

8. **Fuzzy Transform** (`ft.R`, `ftinv.R`):
   - Fuzzy transform computation
   - Inverse fuzzy transform

9. **Sugeno Integral** (`sugeno.R`):
   - Computation of Sugeno integrals

## Common Tasks

### Adding a New Function
1. Create function in appropriate R/*.R file
2. Add GPL-3 copyright header to the file if creating new file
3. Add roxygen2 documentation
4. Export if public function: `@export`
5. Add tests if test infrastructure exists
6. Run `devtools::document()` to update NAMESPACE and man/
7. Run `devtools::check()`

### Fixing a Bug
1. Add a failing test that reproduces the bug (if test infrastructure exists)
2. Fix the code
3. Verify test passes
4. Run `devtools::check()`

### Adding a Vignette
1. Create new .Rmd file in `vignettes/`
2. Add appropriate YAML front matter
3. Include vignette metadata for indexing
4. Build with `devtools::build_vignettes()`

## Best Practices

1. **Never edit generated files**: NAMESPACE, man/*.Rd, RcppExports.cpp/R
2. **Check the package**: Run `devtools::check()` before committing significant changes
3. **Document as you code**: Add roxygen2 comments immediately
4. **Follow existing patterns**: Study similar functions in the codebase before implementing new features
5. **Maintain backward compatibility**: Deprecate functions properly before removal
6. **Keep vignettes current**: Update vignettes when adding significant new features

## CI/CD Workflows

- **rhub**: Additional platform testing
- **pkgdown**: Builds and deploys documentation site to GitHub Pages

## Theoretical Background

The package implements algorithms based on:
- Fuzzy natural logic / linguistic fuzzy logic
- Perception-based logical deduction (PbLD)
- Fuzzy relational calculus
- Linguistic fuzzy association rules
- Fuzzy rule-based ensemble (FRBE)

Key references:
- Novák, V. (2008). A comprehensive theory of trichotomous evaluative linguistic expressions.
- Štěpnička, M., & Burda, M. (2017). Towards a calculus of perceptions.
- Burda, M., & Štěpnička, M. (2017). lfl: An R Package for Linguistic Fuzzy Logic.

## Contributing

Contributions should:
- Follow the existing code style
- Include appropriate documentation
- Pass R CMD check without errors or warnings
- Be compatible with R >= 3.6

## License

GPL-3: This package is free software and comes with ABSOLUTELY NO WARRANTY. You are welcome to redistribute it under certain conditions. See the GNU General Public License for details.
