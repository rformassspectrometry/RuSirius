## Pre-render vignettes that require a running Sirius instance.
##
## This script knits each .Rmd.orig file into the corresponding .Rmd file,
## embedding all output so that pkgdown (and R CMD check) can render them
## without Sirius.
##
## Usage (from the package root directory):
##   Rscript vignettes/pre-render.R
##
## Or from an R session with the working directory at the package root:
##   source("vignettes/pre-render.R")
##
## Requirements:
##   - Sirius 6.3+ running on port 9999 (e.g. inside the Docker container)
##   - All package dependencies installed
##   - A valid Sirius login (for structure database searches)

orig_files <- list.files(
  "vignettes",
  pattern = "\\.Rmd\\.orig$",
  full.names = TRUE
)

if (length(orig_files) == 0L) {
  stop(
    "No .Rmd.orig files found in vignettes/. ",
    "Run this script from the package root directory."
  )
}

message("Found ", length(orig_files), " vignette(s) to pre-render:")
message(paste(" -", basename(orig_files), collapse = "\n"))

for (orig in orig_files) {
  out <- sub("\\.Rmd\\.orig$", ".Rmd", orig)
  message("\n>>> Knitting ", basename(orig), " -> ", basename(out))
  knitr::knit(orig, output = out)
  message("    Done.")
}

message(
  "\nAll vignettes pre-rendered. ",
  "Build the pkgdown site with pkgdown::build_site()."
)
