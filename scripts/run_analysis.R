# Run from the repository root: Rscript scripts/run_analysis.R
# Create the complete dataset before analysis.
source("R/data.R", encoding = "UTF-8")
# Avoid operating-system timezone probes in headless report rendering.
if (!nzchar(Sys.getenv("TZ"))) Sys.setenv(TZ = "UTC")
required <- c("ggplot2", "dplyr", "patchwork")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages with Rscript scripts/install_packages.R: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages(library(ggplot2))
source("config.R", encoding = "UTF-8")
source("R/plot_theme.R", encoding = "UTF-8")
source("R/inference.R", encoding = "UTF-8")
source("R/analysis.R", encoding = "UTF-8")
source("R/figures.R", encoding = "UTF-8")
set.seed(config$seed)
results <- run_analysis(input_data, config)
versions <- data.frame(package = c(required, "knitr", "rmarkdown"),
  version = vapply(c(required, "knitr", "rmarkdown"), function(p) as.character(utils::packageVersion(p)), character(1)))
utils::write.csv(versions, "results/package_versions.csv", row.names = FALSE)
message("Analysis complete: results/tables, results/figures, results/analysis_results.rds")
