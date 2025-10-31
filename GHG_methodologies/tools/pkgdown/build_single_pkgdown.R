#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("The 'here' package is required. Install it with install.packages('here').", call. = FALSE)
  }
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required. Install it with install.packages('fs').", call. = FALSE)
  }
  if (!requireNamespace("pkgdown", quietly = TRUE)) {
    stop("The 'pkgdown' package is required. Install it with install.packages('pkgdown').", call. = FALSE)
  }
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("The 'glue' package is required. Install it with install.packages('glue').", call. = FALSE)
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required. Install it with install.packages('cli').", call. = FALSE)
  }
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("The 'withr' package is required. Install it with install.packages('withr').", call. = FALSE)
  }
  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    stop("The 'pkgbuild' package is required. Install it with install.packages('pkgbuild').", call. = FALSE)
  }
})

source(fs::path("tools", "list_packages.R"))

args <- commandArgs(trailingOnly = TRUE)
pkg_arg <- NULL
for (i in seq_along(args)) {
  if (args[[i]] %in% c("--pkg", "-p")) {
    if ((i + 1) <= length(args)) {
      pkg_arg <- args[[i + 1]]
    }
  }
  if (startsWith(args[[i]], "--pkg=")) {
    pkg_arg <- sub("^--pkg=", "", args[[i]])
  }
}

if (is.null(pkg_arg) || nzchar(pkg_arg) == FALSE) {
  stop("--pkg argument is required (relative path to package directory).", call. = FALSE)
}

root <- here::here()
pkg_dir <- fs::path(root, pkg_arg)
if (!fs::dir_exists(pkg_dir)) {
  stop(glue::glue("Package directory '{pkg_arg}' does not exist."), call. = FALSE)
}

pkg_name <- read.dcf(fs::path(pkg_dir, "DESCRIPTION"), "Package")[1, 1]
cli::cli_inform("Building pkgdown site for {pkg_name} ({pkg_arg})")

local_site_dir <- fs::path(tempdir(), glue::glue("pkgdown-site-{pkg_name}"))
if (fs::dir_exists(local_site_dir)) {
  fs::dir_delete(local_site_dir)
}
fs::dir_create(local_site_dir)

temp_lib_dir <- fs::path(tempdir(), glue::glue("pkgdown-lib-{pkg_name}"))
if (!fs::dir_exists(temp_lib_dir)) {
  fs::dir_create(temp_lib_dir)
}

withr::with_libpaths(temp_lib_dir, action = "prefix", {
  cli::cli_inform("Installing {pkg_name} into temporary library for pkgdown build")
  pkgbuild::install(
    pkg = pkg_dir,
    dest_path = temp_lib_dir,
    clean = TRUE,
    quiet = TRUE
  )

  pkgdown::build_site(
    pkg = pkg_dir,
    preview = FALSE,
    override = list(destination = local_site_dir)
  )
})

should_deploy <- (
  Sys.getenv("GITHUB_REF") == "refs/heads/main" &&
    Sys.getenv("GITHUB_EVENT_NAME") %in% c("push", "workflow_dispatch")
)

if (should_deploy) {
  cli::cli_inform("Deploying pkgdown site for {pkg_name} to gh-pages")
  pkgdown::deploy_to_branch(
    pkg = pkg_dir,
    branch = "gh-pages",
    subdir = fs::path("sites", pkg_name),
    clean = FALSE,
    commit_message = glue::glue("Build pkgdown for {pkg_name}")
  )
} else {
  cli::cli_warn(
    "Skipping deployment for {pkg_name} because this is not a main branch push (ref: {Sys.getenv('GITHUB_REF')}, event: {Sys.getenv('GITHUB_EVENT_NAME')})."
  )
}
