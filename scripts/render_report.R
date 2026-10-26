# Render from the repository root; the Rmd rebuilds the analysis every time.
if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Run Rscript scripts/install_packages.R first.")
# RStudio bundles Pandoc even when it is absent from the shell PATH.
if (!rmarkdown::pandoc_available()) {
  candidates <- c(Sys.getenv("RSTUDIO_PANDOC"),
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64",
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64")
  found <- candidates[file.exists(file.path(candidates, "pandoc"))]
  if (length(found)) Sys.setenv(RSTUDIO_PANDOC = found[1])
}
if (!rmarkdown::pandoc_available()) stop("Pandoc is required. Install Pandoc or render from RStudio.")
rmarkdown::render("reports/analysis.Rmd", output_file = "analysis.html",
  knit_root_dir = normalizePath("."), envir = new.env(parent = globalenv()), encoding = "UTF-8")
