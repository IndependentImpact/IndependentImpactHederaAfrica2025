#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("The 'here' package is required. Install it with install.packages('here').", call. = FALSE)
  }
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required. Install it with install.packages('fs').", call. = FALSE)
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("The 'purrr' package is required. Install it with install.packages('purrr').", call. = FALSE)
  }
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("The 'pkgdown' package is required. Install it with install.packages('pkgdown').", call. = FALSE)
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required. Install it with install.packages('cli').", call. = FALSE)
  }
})

source(fs::path("tools", "list_packages.R"))

root <- here::here()
pkg_dirs <- list_repo_packages(root)

if (length(pkg_dirs) == 0) {
  cli::cli_abort("No packages were discovered in the repository.")
}

docs_root <- fs::path(root, "docs")
if (!fs::dir_exists(docs_root)) {
  fs::dir_create(docs_root)
}

purrr::walk(pkg_dirs, function(pkg_dir) {
  abs_pkg_dir <- fs::path(root, pkg_dir)
  pkg_name <- read.dcf(fs::path(abs_pkg_dir, "DESCRIPTION"), "Package")[1, 1]
  cli::cli_inform("Rendering pkgdown site for local preview: {pkg_name}")

  pkgdown::build_site(
    pkg = abs_pkg_dir,
    preview = FALSE,
    override = list(destination = fs::path(docs_root, pkg_name))
  )
})

cli::cli_inform("All pkgdown sites rendered to 'docs/'. Use pkgdown::serve_site() within a package to preview interactively.")
