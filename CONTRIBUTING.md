# Contributing to MEM5220 - Applied Econometrics

Thank you for your interest in contributing to this teaching material! This document provides guidelines for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Code Standards](#code-standards)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)

## Code of Conduct

Please be respectful and constructive in all interactions. This is an educational resource used by students and instructors.

## Getting Started

### Prerequisites

- R (≥ 4.0.0)
- RStudio (recommended)
- Git
- Basic knowledge of R Markdown and bookdown

### Setting Up Your Development Environment

1. **Fork and clone the repository**
   ```bash
   git clone https://github.com/YOUR-USERNAME/MEM5220.git
   cd MEM5220
   ```

2. **Install required R packages**
   ```r
   # The before-chapter.R script will install all required packages
   # Or install manually:
   install.packages(c("bookdown", "tidyverse", "AER", "plm", "wooldridge"))
   ```

3. **Install pre-commit hooks (optional but recommended)**
   ```bash
   pip install pre-commit
   pre-commit install
   ```

## Development Workflow

### Creating a New Branch

Always create a new branch for your work:

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-description
```

Branch naming conventions:
- `feature/description` - for new content or features
- `fix/description` - for bug fixes
- `docs/description` - for documentation improvements
- `refactor/description` - for code refactoring

### Making Changes

1. **Edit the relevant `.Rmd` files**
   - Use clear, concise language
   - Include working R code examples
   - Add comments to explain complex concepts
   - Test all code chunks

2. **Build the book locally**
   ```r
   bookdown::render_book("index.Rmd", "bookdown::gitbook")
   ```

3. **Preview your changes**
   - Open `_book/index.html` in a browser
   - Check that all code runs without errors
   - Verify formatting and layout

## Code Standards

### R Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/). Key points:

```r
# Good
x <- mean(data$variable, na.rm = TRUE)
my_function <- function(x, y) {
  result <- x + y
  return(result)
}

# Bad
x<-mean(data$variable,na.rm=TRUE)
myFunction=function(x,y){result=x+y;return(result)}
```

### R Markdown Best Practices

1. **Use explicit data frame references**
   ```r
   # Good
   mean(mtcars$mpg)

   # Bad - avoid attach()
   attach(mtcars)
   mean(mpg)
   ```

2. **Set chunk options appropriately**
   ```r
   # For code to show and run
   ```{r, echo=TRUE, eval=TRUE}

   # For code to show but not run
   ```{r, echo=TRUE, eval=FALSE}

   # For code to run but not show
   ```{r, echo=FALSE, eval=TRUE}
   ```

3. **Name your chunks**
   ```r
   ```{r load-data}
   data("mtcars")
   ```

4. **Include package loading at chapter level**
   - Packages are centrally managed in `before-chapter.R`
   - Only load chapter-specific packages in individual files

### Code Formatting

Use the `styler` package to format your code:

```r
# Format a single file
styler::style_file("Panel.Rmd")

# Format all files
styler::style_dir(filetype = "Rmd")
```

### Linting

Check your code with `lintr`:

```r
# Lint a single file
lintr::lint("Panel.Rmd")

# Lint all files
lintr::lint_dir()
```

## Testing

### Test Your Code

1. **Run all code chunks**
   - Ensure every R code chunk executes without errors
   - Use `knitr::purl()` to extract R code from Rmd files for testing

2. **Check for reproducibility**
   - Clear your R environment
   - Run the code from scratch
   - Verify results are consistent

3. **Build the entire book**
   ```r
   bookdown::render_book("index.Rmd", "bookdown::gitbook")
   ```

### Automated Checks

Our CI/CD pipeline will automatically:
- Build the book
- Run linting checks
- Check for broken links

You can run these checks locally before pushing.

## Submitting Changes

### Commit Messages

Write clear, descriptive commit messages:

```bash
# Good
git commit -m "Add quantile regression example to Panel chapter"
git commit -m "Fix typo in GLM probit model section"

# Better - with detailed description
git commit -m "Add quantile regression example to Panel chapter

- Include practical example using wage data
- Add visualization of quantile regression lines
- Reference Koenker (2005) for theoretical background"
```

### Pull Request Process

1. **Update your branch with the latest changes**
   ```bash
   git fetch origin
   git rebase origin/main
   ```

2. **Push your changes**
   ```bash
   git push origin feature/your-feature-name
   ```

3. **Create a Pull Request**
   - Go to the GitHub repository
   - Click "New Pull Request"
   - Select your branch
   - Fill out the PR template with:
     - Clear description of changes
     - Motivation for the changes
     - Testing performed
     - Screenshots (if applicable)

4. **Address review feedback**
   - Respond to comments
   - Make requested changes
   - Push updates to the same branch

### PR Checklist

Before submitting, ensure:

- [ ] All code chunks run without errors
- [ ] Book builds successfully locally
- [ ] Code follows style guidelines
- [ ] New content is properly referenced
- [ ] Commit messages are clear and descriptive
- [ ] Documentation is updated if needed

## Types of Contributions

### Content Improvements

- Add new examples or exercises
- Improve explanations
- Add visualizations
- Update outdated information

### Bug Fixes

- Fix typos and grammatical errors
- Correct code errors
- Fix broken links or references

### Documentation

- Improve README
- Add code comments
- Create tutorials or guides

### Technical Improvements

- Optimize code performance
- Improve book structure
- Enhance build process

## Questions?

If you have questions or need help:

1. Check existing [Issues](https://github.com/nreigl/MEM5220/issues)
2. Open a new Issue with your question
3. Contact the maintainers

## Attribution

Contributors will be acknowledged in the book's acknowledgements section. Please add your name to the contributors list in your PR if you wish to be credited.

## License

By contributing, you agree that your contributions will be licensed under the same license as the project (see [LICENSE](LICENSE)).

---

Thank you for helping improve this educational resource!
