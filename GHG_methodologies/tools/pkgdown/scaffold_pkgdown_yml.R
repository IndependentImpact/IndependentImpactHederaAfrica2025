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
  if (!requireNamespace("whisker", quietly = TRUE)) {
    stop("The 'whisker' package is required. Install it with install.packages('whisker').", call. = FALSE)
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required. Install it with install.packages('cli').", call. = FALSE)
  }
})

source(fs::path("tools", "list_packages.R"))

args <- commandArgs(trailingOnly = TRUE)
force <- any(args %in% c("--force", "-f"))

template_path <- fs::path("tools", "pkgdown", "_pkgdown-template.yml")
if (!fs::file_exists(template_path)) {
  stop("Template file tools/pkgdown/_pkgdown-template.yml not found.", call. = FALSE)
}
template <- readLines(template_path)

root <- here::here()
pkg_dirs <- list_repo_packages(root)
if (length(pkg_dirs) == 0) {
  cli::cli_abort("No packages were discovered in the repository.")
}

github_org <- Sys.getenv("PKGDOWN_GITHUB_ORG", unset = NA_character_)
if (is.na(github_org) || github_org == "") {
  cli::cli_warn("Environment variable PKGDOWN_GITHUB_ORG is not set. Using placeholder 'your-org'.")
  github_org <- "your-org"
}

docsearch_api_key <- Sys.getenv("PKGDOWN_DOCSEARCH_API_KEY", unset = "")
docsearch_index <- Sys.getenv("PKGDOWN_DOCSEARCH_INDEX", unset = "")

purrr::walk(pkg_dirs, function(pkg_dir) {
  abs_pkg_dir <- fs::path(root, pkg_dir)
  desc_path <- fs::path(abs_pkg_dir, "DESCRIPTION")
  desc <- read.dcf(desc_path)
  pkg_name <- desc[, "Package"]
  pkg_title <- desc[, "Title"]
  pkg_description <- desc[, "Description"]

  destination <- fs::path(abs_pkg_dir, "_pkgdown.yml")
  if (fs::file_exists(destination) && !force) {
    cli::cli_inform("Skipping {pkg_name}; _pkgdown.yml already exists.")
    return(invisible())
  }

  data <- list(
    github_org = github_org,
    pkg = pkg_name,
    pkg_path = pkg_dir,
    pkg_title = pkg_title,
    pkg_description = pkg_description,
    docsearch_api_key = docsearch_api_key,
    docsearch_index = docsearch_index,
    build_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  rendered <- whisker::whisker.render(template, data = data)
  cli::cli_inform("Writing _pkgdown.yml for {pkg_name}")
  writeLines(rendered, destination)
})

cli::cli_inform("pkgdown templates scaffolded. Use --force to overwrite existing files.")
