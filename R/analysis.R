# Analyze individuals without weighting their means by particle counts.
life_levels <- c("small_juloid", "middle_juloid", "big_juloid", "polydesmoid", "platydesmoid")
polymer_levels <- c("LDPE", "PET", "PLA", "PP", "PVC")
life_labels <- c(small_juloid = "Small juloid", middle_juloid = "Middle juloid",
                 big_juloid = "Big juloid", polydesmoid = "Polydesmoid", platydesmoid = "Platydesmoid")

prepare_data <- function(data) {
  if (anyDuplicated(data$sample_id)) stop("Individual IDs must be unique.")
  if (anyNA(data$life_form) || any(!data$life_form %in% life_levels)) stop("Unknown or missing life-form labels.")
  if (anyNA(data$polymer) || any(!data$polymer %in% c(polymer_levels, "control"))) stop("Unknown or missing polymer labels.")
  if (anyNA(data$output_count) || any(data$output_count < 0 | data$output_count != floor(data$output_count))) stop("Counts must be nonnegative integers.")
  if (any(!is.na(data$output_size_mm) & (data$output_size_mm <= 0 | data$output_count <= 0))) stop("Analytical sizes require positive size and count.")
  data$life_form <- factor(data$life_form, levels = life_levels)
  data$polymer <- factor(data$polymer, levels = c(polymer_levels, "control"))
  data$input_size_class <- factor(data$input_size_class, levels = c("0.1-0.2", "3-5"))
  data$detected <- data$output_count > 0
  # This ratio is descriptive: no individual input particle size was measured.
  data$size_to_lower_bound <- data$output_size_mm / data$input_lower_mm
  data
}

select_primary <- function(data) {
  droplevels(data[!data$is_control, , drop = FALSE])
}

summarize_groups <- function(data) {
  # Missing sizes remain undefined, even in cells with zero recovered particles.
  safe <- function(x, fun) if (all(is.na(x))) NA_real_ else fun(x[!is.na(x)])
  data |>
    dplyr::group_by(life_form, polymer, input_size_class, is_control, .drop = TRUE) |>
    dplyr::summarise(
      n_individuals = dplyr::n(), n_detected = sum(detected),
      detection_fraction = mean(detected),
      count_mean = mean(output_count), count_sd = stats::sd(output_count),
      count_median = stats::median(output_count), count_min = min(output_count), count_max = max(output_count),
      n_size = sum(!is.na(output_size_mm)),
      size_mean_mm = safe(output_size_mm, mean), size_sd_mm = safe(output_size_mm, stats::sd),
      size_median_mm = safe(output_size_mm, stats::median),
      size_min_mm = safe(output_size_mm, min), size_max_mm = safe(output_size_mm, max),
      .groups = "drop"
    )
}

run_pca <- function(exposure, analysis_name) {
  keep <- exposure$output_count > 0 & is.finite(exposure$output_size_mm) & exposure$output_size_mm > 0
  included <- exposure[keep, , drop = FALSE]
  excluded <- exposure[!keep, c("sample_id", "life_form", "polymer", "input_size_class"), drop = FALSE]
  excluded$reason <- rep("No positive particle-size measurement", nrow(excluded))
  x <- cbind(log_count = log1p(included$output_count), log_mean_size_mm = log(included$output_size_mm))
  if (nrow(x) < 3L || any(apply(x, 2, stats::sd) == 0)) stop("PCA requires at least three complete observations and nonconstant variables.")
  fit <- stats::prcomp(x, center = TRUE, scale. = TRUE)
  scores <- cbind(included[c("sample_id", "life_form", "polymer", "input_size_class")], as.data.frame(fit$x))
  loadings <- data.frame(variable = rownames(fit$rotation), fit$rotation, row.names = NULL)
  variance <- data.frame(component = colnames(fit$x), explained_fraction = fit$sdev^2 / sum(fit$sdev^2))
  preprocessing <- data.frame(variable = colnames(x), center = fit$center, scale = fit$scale, row.names = NULL)
  list(name = analysis_name, fit = fit, scores = scores, loadings = loadings,
       variance = variance, preprocessing = preprocessing, excluded = excluded)
}

run_analysis <- function(data, config, output_dir = "results") {
  data <- prepare_data(data)
  primary <- select_primary(data)
  exposure <- data[!data$is_control, ]
  group_summary <- summarize_groups(data)
  primary_pca <- run_pca(primary, "primary")
  inference <- run_inference(primary, config)
  omnibus <- run_omnibus(primary, config)
  plots <- make_figures(data, primary, primary_pca, inference, omnibus)
  for (folder in c("tables", "figures", "models")) dir.create(file.path(output_dir, folder), recursive = TRUE, showWarnings = FALSE)
  tables <- list(group_summary = group_summary, primary_contrasts = inference$contrasts,
    primary_test_families = inference$family_summary, primary_skipped_tests = inference$skipped,
    omnibus_life_form_tests = omnibus,
    pca_scores = primary_pca$scores, pca_loadings = primary_pca$loadings,
    pca_variance = primary_pca$variance, pca_preprocessing = primary_pca$preprocessing,
    pca_excluded = primary_pca$excluded,
    primary_inclusion = data.frame(sample_id = exposure$sample_id, included = exposure$sample_id %in% primary$sample_id))
  for (name in names(tables)) utils::write.csv(tables[[name]], file.path(output_dir, "tables", paste0(name, ".csv")), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  for (name in names(plots)) {
    dimensions <- figure_dimensions(name)
    save_mp_plot(plots[[name]], file.path(output_dir, "figures", name),
      width = dimensions[["width"]], height = dimensions[["height"]], dpi = config$figure_dpi)
  }
  saveRDS(primary_pca$fit, file.path(output_dir, "models", "pca.rds"))
  result <- list(config = config, data = data, primary = primary, summary = group_summary,
    inference = inference, omnibus = omnibus, pca = primary_pca, plots = plots)
  saveRDS(result, file.path(output_dir, "analysis_results.rds"))
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
  result
}
