# Install only missing R packages; never change user library settings.
packages <- c("ggplot2", "dplyr", "patchwork", "knitr", "rmarkdown")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Required packages are available. Pandoc is also needed for HTML rendering.")
