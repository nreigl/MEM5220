# LIBRARY CONFIGURATION
# This script is run before each chapter is rendered
# It loads all necessary packages for the MEM5220 Applied Econometrics book

# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  tryCatch({
    install.packages("pacman", repos = "https://cran.rstudio.com")
  }, error = function(e) {
    stop("Failed to install pacman package: ", e$message)
  })
}

# Comprehensive package list for all chapters
PACKAGES <- c(
  # Core R packages
  "pacman",

  # Data manipulation
  "tidyverse", "dplyr", "tibble", "magrittr", "bindrcpp",
  "pmdplyr", "collapse",

  # Visualization
  "ggplot2", "ggpubr", "ggfortify", "plot3D", "scales",

  # Econometrics - General
  "AER", "car", "lmtest", "estimatr",

  # Econometrics - Panel Data
  "plm", "pglm", "splm", "pder", "fixest", "lfe", "lme4",

  # Econometrics - Time Series
  "dynlm", "fUnitRoots", "uroot", "urca", "forecast", "astsa",
  "eurostat", "stats",

  # Econometrics - GLM and LDV
  "nnet", "survival", "sampleSelection", "censReg",

  # Bayesian methods
  "MCMCpack",

  # Regression tables and output
  "broom", "stargazer", "huxtable", "modelsummary", "texreg",
  "knitr", "kableExtra", "summarytools",

  # Statistical methods
  "MASS", "mvtnorm", "quantreg", "Hmisc", "psych",

  # Missing data
  "mice", "VIM",

  # Outliers and robust statistics
  "OutliersO3", "robustbase",

  # Modeling and simulation
  "modelr", "tidymodels",

  # Prediction and margins
  "margins", "prediction",

  # Data packages
  "wooldridge", "PoEdata",

  # Helper packages
  "skimr", "stringr"
)

# Install missing packages with error handling
inst <- match(PACKAGES, .packages(all = TRUE))
need <- which(is.na(inst))

if (length(need) > 0) {
  message("Installing ", length(need), " missing package(s)...")
  tryCatch({
    install.packages(PACKAGES[need], repos = "https://cran.rstudio.com")
  }, error = function(e) {
    warning("Some packages failed to install: ", e$message)
    warning("Missing packages: ", paste(PACKAGES[need], collapse = ", "))
  })
}

# Load all packages quietly with error handling
loaded <- suppressPackageStartupMessages({
  sapply(PACKAGES, function(pkg) {
    tryCatch({
      require(pkg, character.only = TRUE, quietly = TRUE)
    }, error = function(e) {
      warning("Failed to load package '", pkg, "': ", e$message)
      FALSE
    })
  })
})

# Report any packages that failed to load
failed <- PACKAGES[!loaded]
if (length(failed) > 0) {
  warning("The following packages failed to load: ", paste(failed, collapse = ", "))
}

# KNITR CONFIGURATION
library(dplyr)
library(ggplot2)
library(knitr)

knitr::opts_chunk$set(
  cache = TRUE,
  collapse = TRUE,
  comment = NA,
  dev = "png",
  dpi = 150,
  echo = TRUE,
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,  # 1 / phi (golden ratio)
  fig.show = "hold",
  include = TRUE,
  message = FALSE,
  out.width = "70%",
  tidy = FALSE,
  warning = FALSE
)

# UTILITY FUNCTIONS

#' Print formatted text
#'
#' A wrapper around sprintf and cat for convenient formatted printing
#'
#' @param pattern A character string with format specifiers
#' @param ... Values to substitute into pattern
#' @return NULL (invisibly). Prints to console as side effect.
#' @examples
#' printf("The mean is %.2f", 3.14159)
printf <- function(pattern, ...) {
  cat(sprintf(pattern, ...))
}

#' Print contents of a file
#'
#' Reads and prints all lines from a file
#'
#' @param file Path to the file to read
#' @return NULL (invisibly). Prints to console as side effect.
#' @examples
#' print_file("README.md")
print_file <- function(file) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  tryCatch({
    cat(paste(readLines(file), "\n", sep = ""), sep = "")
  }, error = function(e) {
    stop("Error reading file '", file, "': ", e$message)
  })
}

# GENERAL R CONFIGURATION
options(htmltools.dir.version = FALSE)

# Set seed for reproducibility (can be overridden in individual chapters)
SEED_FOR_REPRODUCIBILITY <- 42
set.seed(SEED_FOR_REPRODUCIBILITY)

# Log package loading completion
message("Package loading complete. ", sum(loaded), "/", length(PACKAGES), " packages loaded successfully.")
