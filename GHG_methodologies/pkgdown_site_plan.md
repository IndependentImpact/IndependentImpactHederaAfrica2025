# pkgdown + GitHub Pages Implementation Plan

This plan describes how to automate pkgdown site generation for **every** CDM methodology package in this repository using GitHub Pages. All steps rely on scripted workflows with `pkgdown`, `usethis`, and custom helper code—no manual GitHub UI actions are required.

## 1. Repository-wide preparation

1. **Enumerate packages programmatically**
   - Add an R helper (`tools/list_packages.R`) that scans the repository for `DESCRIPTION` files and returns the package directories. Example skeleton:

     ```r
     list_repo_packages <- function(root = here::here()) {
       desc <- fs::dir_ls(root, recurse = TRUE, glob = "**/DESCRIPTION", type = "file")
       fs::path_dir(desc)
     }
     ```

   - Use `here`, `fs`, and `purrr` to keep path handling portable.

2. **Ensure pkgdown metadata is present**
   - For each package directory, call `usethis::use_pkgdown()` to scaffold `_pkgdown.yml` (skipping existing files).
   - Populate `_pkgdown.yml` with a shared template (stored in `tools/pkgdown/_pkgdown-template.yml`) that standardises navbar links, authorship, and reference index structure. During deployment, copy the template and inject package-specific values (name, URL, authors) with `whisker` or `glue`.
   - Confirm that every package has a `README.Rmd` and vignette listed in `_pkgdown.yml` so the site includes core methodology documentation.

3. **Link pkgdown to GitHub Pages branch**
   - Inside each package, run `usethis::use_pkgdown_github_pages(branch = "gh-pages", clean = FALSE)` to create the standard deployment workflow without touching the UI.
   - After scaffolding, programmatically edit the generated `.github/workflows/pkgdown.yaml` (see §2) so the deploy step calls `pkgdown::deploy_to_branch(dest_dir = file.path("sites", pkg_name), clean = FALSE)`; this keeps every package’s rendered site inside `sites/<package>` on the shared `gh-pages` branch and yields URLs like `https://<org>.github.io/GHG_methodologies/sites/<package>/`.

4. **Register pkgdown build dependencies**
   - Add `pkgdown` (and `whisker` if templating) to the `Suggests` field in each `DESCRIPTION` using `usethis::use_package("pkgdown", type = "Suggests")`.
   - Update each package’s `README.Rmd` badge block to include a pkgdown site badge referencing the eventual GitHub Pages URL.

## 2. Automated build scripts

1. **Central build orchestrator**
   - Create `tools/pkgdown/build_all_pkgdown.R` that:
     1. sources `list_packages.R`,
     2. loops over packages with `purrr::walk()`,
     3. runs `pkgdown::build_site(pkg = pkg_dir, override = list(destination = fs::path("../docs", pkg_name)))` to stage output locally,
     4. calls `pkgdown::deploy_to_branch()` with `branch = "gh-pages"`, `dest_dir = fs::path("sites", pkg_name)`, and `clean = FALSE` to push artefacts.

   - Example loop body:

     ```r
     purrr::walk(pkg_dirs, function(pkg_dir) {
       pkgdown::build_site(pkg = pkg_dir, override = list(destination = "site"))
       pkgdown::deploy_to_branch(
         pkg = pkg_dir,
         branch = "gh-pages",
         dest_dir = fs::path("sites", basename(pkg_dir)),
         clean = FALSE,
         new_process = FALSE
       )
     })
     ```

2. **Cache and reuse artefacts**
   - Configure the script to reuse the `site/` build directory between packages to reduce build time (e.g., set `destination = tempdir()` and clean afterwards).
   - Ensure `deploy_to_branch()` commits with informative messages like `"Build pkgdown for {pkg_name}"` using the `commit_message` argument.

3. **Local verification helper**
   - Provide `tools/pkgdown/preview_all_pkgdown.R` that renders every site into `docs/` for local inspection without pushing. Use `pkgdown::build_site(preview = FALSE)` and then run `pkgdown::serve_site()` for the selected package when developers want to preview.

## 3. Continuous deployment via GitHub Actions

1. **Create composite workflow**
   - Add `.github/workflows/pkgdown-matrix.yml` with a matrix over all packages returned by `tools/list_packages.R`. Steps:
     - Checkout code.
     - Setup R (using `r-lib/actions/setup-r@v2`).
     - Install dependencies with `r-lib/actions/setup-r-dependencies@v2` (pointing to each package path).
     - Run the orchestrator script for the current matrix package, e.g. `Rscript tools/pkgdown/build_single_pkgdown.R --pkg ${{ matrix.package }}`.

2. **Matrix-friendly build helper**
   - Create `tools/pkgdown/build_single_pkgdown.R` that accepts a `--pkg` argument, builds only that package, and deploys it to `gh-pages/sites/<pkg>` using the same `deploy_to_branch()` call.
   - Guard the script so it only pushes when `GITHUB_REF == "refs/heads/main"` and `GITHUB_EVENT_NAME` is `push` or `workflow_dispatch`.

3. **GitHub token management**
   - In the workflow, configure `GITHUB_PAT` from the default `GITHUB_TOKEN`. `pkgdown::deploy_to_branch()` recognises it automatically; no UI configuration needed.

4. **Pages configuration**
   - Include a one-time scripted step in the workflow to ensure the `gh-pages` branch exists and contains a `CNAME` (if using a custom domain). Example shell step before deployment:

     ```bash
     git fetch origin gh-pages || echo "gh-pages does not exist yet"
     ```

   - After first deployment, GitHub Pages automatically serves the branch; no UI toggles required because `use_pkgdown_github_pages()` creates `.github/workflows/pkgdown.yaml` with the correct `pages-build-deployment` metadata.

## 4. Documentation and onboarding

1. **Developer guide**
   - Update `PACKAGE_DEVELOPMENT_TEMPLATE.md` with a “pkgdown publishing” section referencing the new scripts and expected URLs.
   - Document how to trigger manual rebuilds via `gh workflow run pkgdown-matrix --ref main`.

2. **Monitoring**
   - Optionally add a status badge for the pkgdown workflow to each package README (use `usethis::use_github_action_badge("pkgdown-matrix")`).
   - Configure Renovate or Dependabot to keep `pkgdown` and `usethis` dependencies current, ensuring builds stay compatible.

Following these steps will deliver automated, repeatable pkgdown site generation for every methodology package without touching the GitHub web interface.
