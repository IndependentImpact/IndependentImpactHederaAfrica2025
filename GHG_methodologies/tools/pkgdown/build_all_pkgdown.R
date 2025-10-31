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
  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("The 'glue' package is required. Install it with install.packages('glue').", call. = FALSE)
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required. Install it with install.packages('cli').", call. = FALSE)
  }
  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    stop("The 'pkgbuild' package is required. Install it with install.packages('pkgbuild').", call. = FALSE)
  }
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("The 'withr' package is required. Install it with install.packages('withr').", call. = FALSE)
  }
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("The 'gert' package is required. Install it with install.packages('gert').", call. = FALSE)
  }
})

source(fs::path("tools", "list_packages.R"))

root <- here::here()
pkg_dirs <- list_repo_packages(root)

if (length(pkg_dirs) == 0) {
  cli::cli_abort("No packages were discovered in the repository.")
}

pkg_info <- purrr::map(pkg_dirs, function(pkg_dir) {
  abs_pkg_dir <- fs::path(root, pkg_dir)
  pkg_name <- read.dcf(fs::path(abs_pkg_dir, "DESCRIPTION"), "Package")[1, 1]

  list(
    name = pkg_name,
    dir = pkg_dir,
    abs_dir = abs_pkg_dir
  )
})

build_index_html <- function(pkg_info) {
  escape_html <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text <- gsub("\"", "&quot;", text, fixed = TRUE)
    gsub("'", "&#39;", text, fixed = TRUE)
  }

  extract_method_code <- function(title, description) {
    candidates <- c(title, description)
    for (candidate in candidates) {
      if (is.na(candidate) || candidate == "") {
        next
      }

      acm_match <- regexpr("ACM[0-9]{4}", candidate, perl = TRUE)
      if (acm_match != -1) {
        return(regmatches(candidate, acm_match))
      }

      ams_match <- regexpr("AMS-[IVX]+\\.[A-Z]+", candidate, perl = TRUE)
      if (ams_match != -1) {
        return(regmatches(candidate, ams_match))
      }
    }

    NA_character_
  }

  extract_method_label <- function(description) {
    if (is.na(description) || description == "") {
      return(NA_character_)
    }

    quote_match <- regexpr('\"([^\"]+)\"', description, perl = TRUE)
    if (quote_match == -1) {
      return(NA_character_)
    }

    gsub('"', '', regmatches(description, quote_match), fixed = TRUE)
  }

  normalise_space <- function(text) {
    gsub("\\s+", " ", text)
  }

  pkg_entries <- purrr::map(pkg_info, function(info) {
    desc_path <- fs::path(info$abs_dir, "DESCRIPTION")
    fields <- read.dcf(desc_path, fields = c("Title", "Description"))
    title <- fields[1, "Title"]
    description <- fields[1, "Description"]
    description <- normalise_space(description)

    if (grepl("CDMLargeScale", info$dir, fixed = TRUE)) {
      scale <- "large-scale"
    } else if (grepl("CDMSmallScale", info$dir, fixed = TRUE)) {
      scale <- "small-scale"
    } else {
      scale <- "other"
    }

    method_code <- extract_method_code(title, description)
    method_label <- extract_method_label(description)

    scale_phrase <- if (scale %in% c("large-scale", "small-scale")) {
      glue::glue("{scale} methodology")
    } else {
      "methodology"
    }

    summary <- if (!is.na(method_code) && !is.na(method_label)) {
      glue::glue(
        "{info$name} implements the Clean Development Mechanism (CDM) {scale_phrase} {method_code} {method_label}."
      )
    } else if (!is.na(method_code)) {
      glue::glue(
        "{info$name} implements the Clean Development Mechanism (CDM) {scale_phrase} {method_code}."
      )
    } else {
      glue::glue(
        "{info$name} provides tooling for the Clean Development Mechanism (CDM) {scale_phrase}."
      )
    }

    list(
      scale = scale,
      name = info$name,
      dir = info$dir,
      code = method_code,
      summary = normalise_space(summary)
    )
  })

  pkg_entries <- pkg_entries[order(tolower(purrr::map_chr(pkg_entries, "name")))]
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  section_order <- c("large-scale", "small-scale", "other")
  section_titles <- c(
    "large-scale" = "Large-scale methodologies (ACM)",
    "small-scale" = "Small-scale methodologies (AMS)",
    "other" = "Other packages"
  )

  section_content <- purrr::map(section_order, function(section) {
    section_entries <- purrr::keep(pkg_entries, ~.x$scale == section)
    if (length(section_entries) == 0) {
      return(NULL)
    }

    items <- purrr::map_chr(section_entries, function(entry) {
      code_badge <- if (!is.na(entry$code)) {
        glue::glue("<span class=\"package-code\">{escape_html(entry$code)}</span>")
      } else {
        ""
      }

      glue::glue(
        "      <li>\n        <div class=\"package-header\">\n          <a href=\"sites/{escape_html(entry$name)}/\">{escape_html(entry$name)}</a>\n          {code_badge}\n        </div>\n        <p class=\"package-summary\">{escape_html(entry$summary)}</p>\n        <span class=\"package-path\">{escape_html(entry$dir)}</span>\n      </li>"
      )
    })

    c(
      glue::glue("    <section>"),
      glue::glue("      <h2>{section_titles[[section]]}</h2>"),
      "      <ul>",
      items,
      "      </ul>",
      "    </section>"
    )
  })

  section_content <- purrr::compact(section_content)
  section_content <- if (length(section_content) == 0) {
    character()
  } else {
    unlist(section_content, use.names = FALSE)
  }

  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"utf-8\" />",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />",
    "  <title>GHG Methodologies pkgdown sites</title>",
    "  <style>",
    "    body { font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 0; padding: 2rem; background: #f7f7f7; color: #222; }",
    "    main { max-width: 1024px; margin: 0 auto; background: white; padding: 2.5rem; border-radius: 12px; box-shadow: 0 10px 30px rgba(0, 0, 0, 0.08); }",
    "    h1 { font-size: 2.25rem; margin-bottom: 0.5rem; }",
    "    h2 { margin-top: 2.5rem; font-size: 1.5rem; border-bottom: 1px solid #e5e5e5; padding-bottom: 0.5rem; }",
    "    p.meta { color: #555; margin-top: 0; }",
    "    section:first-of-type h2 { margin-top: 2rem; }",
    "    ul { list-style: none; padding: 0; margin: 1.5rem 0 0; display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 1.25rem; }",
    "    li { background: #fafafa; border: 1px solid #e5e5e5; border-radius: 10px; padding: 1.1rem 1.35rem; transition: transform 0.2s ease, box-shadow 0.2s ease; display: flex; flex-direction: column; gap: 0.75rem; }",
    "    li:hover { transform: translateY(-3px); box-shadow: 0 12px 20px rgba(0, 0, 0, 0.08); }",
    "    .package-header { display: flex; align-items: center; justify-content: space-between; gap: 0.75rem; }",
    "    a { color: #005a9c; font-weight: 600; text-decoration: none; font-size: 1.1rem; flex-grow: 1; }",
    "    a:hover, a:focus { text-decoration: underline; }",
    "    .package-code { display: inline-block; font-size: 0.85rem; font-weight: 600; color: #1a1a1a; background: #e3ecf7; border: 1px solid #c6d8f0; border-radius: 999px; padding: 0.2rem 0.6rem; letter-spacing: 0.03em; }",
    "    .package-summary { margin: 0; color: #333; line-height: 1.45; }",
    "    .package-path { display: inline-block; font-family: 'Fira Code', 'Source Code Pro', monospace; font-size: 0.85rem; color: #555; background: #eef2f7; padding: 0.2rem 0.55rem; border-radius: 6px; align-self: flex-start; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <main>",
    "    <h1>GHG Methodologies pkgdown sites</h1>",
    glue::glue("    <p class=\"meta\">Last updated {now}. Links point to each package's pkgdown site on GitHub Pages.</p>"),
    section_content,
    "  </main>",
    "</body>",
    "</html>"
  )
}

update_pkgdown_index <- function(root, pkg_info) {
  cli::cli_inform("Updating pkgdown index page")

  worktree_dir <- fs::path(tempdir(), "gh-pages-index")

  if (fs::dir_exists(worktree_dir)) {
    fs::dir_delete(worktree_dir)
  }

  worktree_created <- FALSE

  cleanup_worktree <- function() {
    if (worktree_created) {
      try(
        withr::with_dir(
          root,
          system2(
            "git",
            c("worktree", "remove", "--force", worktree_dir),
            stdout = FALSE,
            stderr = FALSE
          )
        ),
        silent = TRUE
      )
    }

    if (fs::dir_exists(worktree_dir)) {
      try(fs::dir_delete(worktree_dir), silent = TRUE)
    }
  }

  on.exit(cleanup_worktree(), add = TRUE)

  add_result <- tryCatch(
    withr::with_dir(
      root,
      system2(
        "git",
        c("worktree", "add", worktree_dir, "gh-pages"),
        stdout = TRUE,
        stderr = TRUE
      )
    ),
    error = function(err) {
      cli::cli_warn("Skipping index update because the 'gh-pages' worktree could not be created: {err$message}")
      return(NULL)
    }
  )

  if (is.null(add_result)) {
    return(invisible(NULL))
  }

  status_code <- attr(add_result, "status")

  if (!is.null(status_code) && status_code != 0) {
    cli::cli_warn(
      "Skipping index update because the 'gh-pages' worktree could not be created: {paste(add_result, collapse = '\n')}"
    )
    return(invisible(NULL))
  }

  worktree_created <- TRUE

  index_html <- build_index_html(pkg_info)
  index_path <- fs::path(worktree_dir, "index.html")
  writeLines(index_html, index_path)

  if (nrow(gert::git_status(repo = worktree_dir)) == 0) {
    cli::cli_inform("Index page already up to date")
    return(invisible(NULL))
  }

  gert::git_add(repo = worktree_dir, files = "index.html")

  committed <- tryCatch(
    {
      gert::git_commit(repo = worktree_dir, message = "Update pkgdown index")
      TRUE
    },
    error = function(err) {
      cli::cli_warn("Unable to commit index update: {err$message}")
      FALSE
    }
  )

  if (!committed) {
    return(invisible(NULL))
  }

  remotes <- tryCatch(
    gert::git_remote_list(repo = worktree_dir),
    error = function(err) {
      cli::cli_warn("Unable to list git remotes for pushing: {err$message}")
      return(data.frame())
    }
  )

  if (nrow(remotes) == 0) {
    cli::cli_inform("No git remotes configured; skipping push of index update")
    return(invisible(NULL))
  }

  tryCatch(
    gert::git_push(repo = worktree_dir),
    error = function(err) {
      cli::cli_warn("Failed to push index update: {err$message}")
    }
  )
}

purrr::walk(pkg_info, function(info) {
  abs_pkg_dir <- info$abs_dir
  pkg_dir <- info$dir
  pkg_name <- info$name
  cli::cli_inform("Building source package for {pkg_name} ({pkg_dir})")

  pkg_tarball <- pkgbuild::build(
    path = abs_pkg_dir,
    dest_path = tempdir(),
    quiet = TRUE
  )

  temp_lib <- fs::path(tempdir(), glue::glue("lib-{pkg_name}"))

  if (fs::dir_exists(temp_lib)) {
    fs::dir_delete(temp_lib)
  }
  fs::dir_create(temp_lib)

  utils::install.packages(
    pkg_tarball,
    repos = NULL,
    type = "source",
    lib = temp_lib,
    quiet = TRUE
  )

  cli::cli_inform("Building pkgdown site for {pkg_name} ({pkg_dir})")

  local_site_dir <- fs::path(tempdir(), glue::glue("pkgdown-site-{pkg_name}"))

  if (fs::dir_exists(local_site_dir)) {
    fs::dir_delete(local_site_dir)
  }
  fs::dir_create(local_site_dir)

  withr::with_libpaths(temp_lib, action = "prefix", {
    pkgdown::build_site(
      pkg = abs_pkg_dir,
      override = list(destination = local_site_dir),
      preview = FALSE
    )

    pkgdown::deploy_to_branch(
      pkg = abs_pkg_dir,
      branch = "gh-pages",
      subdir = fs::path("sites", pkg_name),
      clean = FALSE,
      commit_message = glue::glue("Build pkgdown for {pkg_name}")
    )
  })

  if (fs::dir_exists(temp_lib)) {
    fs::dir_delete(temp_lib)
  }

  if (fs::file_exists(pkg_tarball)) {
    fs::file_delete(pkg_tarball)
  }
})

update_pkgdown_index(root, pkg_info)

cli::cli_inform("All pkgdown sites built and deployed.")
