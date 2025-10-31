# Ensure cdmAmsIId functions are available when tests are run outside
# an installed package context (e.g., via testthat::test_dir).
# When the package is already loaded (e.g., under test_check), this helper
# is a no-op.

if (!"cdmAmsIId" %in% loadedNamespaces()) {
  find_pkg_root <- function(path = getwd()) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    repeat {
      desc_path <- file.path(path, "DESCRIPTION")
      if (file.exists(desc_path)) {
        desc <- readLines(desc_path, warn = FALSE)
        pkg_line <- grep("^Package:", desc, value = TRUE)
        if (length(pkg_line) && grepl("\\bcdmAmsIId\\b", pkg_line[1])) {
          return(path)
        }
      }
      parent <- dirname(path)
      if (identical(parent, path)) {
        stop("Package root for cdmAmsIId not found.", call. = FALSE)
      }
      path <- parent
    }
  }

  pkg_root <- find_pkg_root()
  r_dir <- file.path(pkg_root, "R")
  r_files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  invisible(lapply(r_files, sys.source, envir = globalenv()))
}
