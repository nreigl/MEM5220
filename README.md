# MEM5220 - Applied Econometrics

This repository holds the R bookdown source code for the course **MEM5220 - Applied Econometrics** at [Tallinn University of Technology, Department of Economics and Finance](https://taltech.ee/en/department-economics-and-finance).

## About

This teaching material provides comprehensive coverage of applied econometrics topics using R, including:

- Linear Regression Models
- Panel Data Models
- Generalized Linear Models (GLM)
- Instrumental Variables (IV)
- Time Series Analysis
- Bayesian Methods
- Special Topics (Difference-in-Differences, etc.)

The book is designed as a companion to classroom instruction and includes practical examples, R code, and interactive exercises.

## Prerequisites

- Basic knowledge of R programming
- Understanding of fundamental statistics
- R (≥ 4.0.0) and RStudio installed

## Installation

### 1. Install R and RStudio

- [Download R](http://cran.r-project.org) for your operating system
- [Download RStudio](https://www.rstudio.com) IDE

### 2. Clone this repository

```bash
git clone https://github.com/nreigl/MEM5220.git
cd MEM5220
```

### 3. Install required R packages

The necessary packages will be automatically installed when you build the book. Key packages include:

- **Data manipulation**: tidyverse, dplyr
- **Econometrics**: AER, plm, fixest, lfe, wooldridge
- **Visualization**: ggplot2, ggpubr
- **Tables**: stargazer, huxtable, modelsummary
- **Time series**: dynlm, forecast, urca
- **And many more** (see individual chapter files for complete lists)

## Building the Book

### Build HTML version

```r
bookdown::render_book("index.Rmd", "bookdown::gitbook")
```

### Build PDF version

```r
bookdown::render_book("index.Rmd", "bookdown::pdf_book")
```

The compiled book will be available in the `_book/` directory.

## Project Structure

```
MEM5220/
├── index.Rmd           # Introduction and setup
├── LM.Rmd              # Linear Regression
├── Panel.Rmd           # Panel Data Models
├── GLM.Rmd             # Generalized Linear Models
├── IV.Rmd              # Instrumental Variables
├── TimeSeries.Rmd      # Time Series Analysis
├── Bayes.Rmd           # Bayesian Methods
├── special_topics.Rmd  # Special Topics
├── references.Rmd      # Bibliography
├── _bookdown.yml       # Book configuration
├── _output.yml         # Output format settings
├── before-chapter.R    # Pre-chapter setup script
├── images/             # Figures and images
└── problem-sets/       # Self-evaluation exercises
```

## Resources

This material builds primarily on:

- **Heiss (2016)**: [Using R for Introductory Econometrics](http://www.urfie.net/read.html)
- **Wooldridge (2015)**: Introductory Econometrics: A Modern Approach

Additional resources and attributions are listed in the book's Introduction.

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Make your changes
4. Commit your changes (`git commit -m 'Add improvement'`)
5. Push to the branch (`git push origin feature/improvement`)
6. Open a Pull Request

### Reporting Issues

If you find errors or have suggestions, please [open an issue](https://github.com/nreigl/MEM5220/issues).

## Version History

### Version 3.1.0 (March 15, 2021)
- Cleanup of outdated functions (dplyr::arrange)
- Removed `renv` dependency management
- Added quantile regression subsection to Linear Models
- Added heteroskedasticity simulations

### Known Issues
- Line 2329 in LM.Rmd: `waldtest` example commented out due to compilation issue

## License

See [LICENSE](LICENSE) file for details.

## Acknowledgements

Special thanks to:
- Kadri Männasoo and Juan Carlos Cuestas for valuable feedback
- The R community and package developers
- Contributors to open-source econometrics teaching materials

## Author

**Nicolas Reigl**
Tallinn University of Technology
Department of Economics and Finance

## Additional Resources

- [Git for Economists](https://github.com/nreigl/git-for-economists) - Introduction to Git/GitHub
- [Happy Git and GitHub for the useR](https://happygitwithr.com/) - Comprehensive Git guide for R users

---

For questions about the course material, please contact the instructor or open an issue on GitHub.
