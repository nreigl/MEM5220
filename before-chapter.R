# LIBRARY CONFIGURATION
# This script is run before each chapter is rendered
# It loads all necessary packages for the MEM5220 Applied Econometrics book

# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cran.rstudio.com")
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

# Install missing packages
inst <- match(PACKAGES, .packages(all = TRUE))
need <- which(is.na(inst))
if (length(need) > 0) {
  install.packages(PACKAGES[need], repos = "https://cran.rstudio.com")
}

# Load all packages quietly
suppressPackageStartupMessages({
  lapply(PACKAGES, require, character.only = TRUE, quietly = TRUE)
})

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
printf <- function(pattern, ...) {
  cat(sprintf(pattern, ...))
}

print_file <- function(file) {
  cat(paste(readLines(file), "\n", sep = ""), sep = "")
}

# GENERAL R CONFIGURATION
options(htmltools.dir.version = FALSE)

# Set seed for reproducibility (can be overridden in individual chapters)
set.seed(42)
