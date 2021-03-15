# LIBRARY CONFIGURATION

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
  fig.asp = 0.618,  # 1 / phi
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
  cat(paste(readLines(file), "\n", sep=""), sep="")
}

# GENERAL R CONFIGURATION

# options(digits = 2)
options(htmltools.dir.version = FALSE)


# set.seed(1123)
