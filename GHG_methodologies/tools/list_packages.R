#' List all R package directories in the repository
#'
#' This helper inspects the repository for DESCRIPTION files and returns the
#' directories that contain them. Paths are returned relative to the repository
#' root to make them easier to use in scripts and GitHub Actions matrices.
list_repo_packages <- function(root = NULL) {
  if (is.null(root)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      stop("The 'here' package must be installed to determine the repository root.", call. = FALSE)
    }
    root <- here::here()
  }
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package must be installed to use list_repo_packages().", call. = FALSE)
  }

  desc_files <- fs::dir_ls(
    root,
    recurse = TRUE,
    glob = "**/DESCRIPTION",
    type = "file"
  )

  pkg_dirs <- fs::path_dir(desc_files)
  fs::path_rel(pkg_dirs, start = root)
}

#' List package names from DESCRIPTION directories
#'
#' @param root Repository root.
#' @return Named character vector where names are package names and values are
#'   relative paths.
list_repo_package_names <- function(root = NULL) {
  if (is.null(root)) {
    if (!requireNamespace("here", quietly = TRUE)) {
      stop("The 'here' package must be installed to determine the repository root.", call. = FALSE)
    }
    root <- here::here()
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("The 'purrr' package must be installed to use list_repo_package_names().", call. = FALSE)
  }

  pkg_dirs <- list_repo_packages(root)
  purrr::set_names(
    pkg_dirs,
    purrr::map_chr(pkg_dirs, ~ read.dcf(fs::path(root, ., "DESCRIPTION"), "Package"))
  )
}
