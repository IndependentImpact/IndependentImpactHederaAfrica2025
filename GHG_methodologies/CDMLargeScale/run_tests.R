#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(testthat))

args <- commandArgs(trailingOnly = FALSE)
script_path <- dirname(sub("^--file=", "", args[grep("^--file=", args)]))
if (length(script_path) == 0 || script_path == ".") {
  script_path <- getwd()
}
script_path <- normalizePath(script_path)

package_dirs <- dir(script_path, full.names = TRUE)
package_dirs <- package_dirs[file.info(package_dirs)$isdir]
package_dirs <- sort(package_dirs)

if (length(package_dirs) == 0) {
  stop(sprintf("No package directories found in %s", script_path))
}

failures <- character()

for (pkg in package_dirs) {
  pkg_name <- basename(pkg)
  message(sprintf("\n==> Running tests for %s", pkg_name))
  result <- try(testthat::test_local(pkg), silent = TRUE)
  if (inherits(result, "try-error")) {
    message(sprintf("Tests failed for %s", pkg_name))
    failures <- c(failures, pkg_name)
  }
}

if (length(failures) > 0) {
  message(sprintf("\nTests failed for %s", paste(failures, collapse = ", ")))
  quit(status = 1)
} else {
  message("\nAll tests passed for Large-Scale methodologies.")
}
