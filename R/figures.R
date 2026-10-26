# Keep figure text to axes, group labels, legends, and numerical p-values.
# Methodological details belong in the report and future figure captions.

mp_format_p <- function(x) {
  vapply(x, function(value) {
    if (!is.finite(value)) return("NA")
    formatC(value, digits = 3, format = "fg", flag = "#") |>
      sub("0+$", "", x = _) |>
      sub("\\.$", "", x = _)
  }, character(1))
}

mp_p_expression <- function(x) {
  paste0("p[BH] == ", ifelse(is.finite(x), mp_format_p(x), "'NA'"))
}

figure_dimensions <- function(name) {
  if (name %in% c("size", "count_size")) return(c(width = 13, height = 9))
  if (name == "counts") return(c(width = 13, height = 7))
  if (grepl("_pvalues$", name)) return(c(width = 13, height = 8))
  if (name %in% c("size_effects", "count_effects")) return(c(width = 15, height = 5.5))
  if (name == "pca_scores") return(c(width = 12, height = 6))
  if (name == "controls") return(c(width = 9, height = 4.6))
  c(width = 9, height = 5.5)
}

make_figures <- function(data, primary, pca, inference, omnibus) {
  exposure <- data[!data$is_control, , drop = FALSE]
  exposure$life_form <- factor(exposure$life_form, levels = life_levels)
  exposure$polymer <- factor(exposure$polymer, levels = polymer_levels)
  input_levels <- c("0.1-0.2", "3-5")
  exposure$input_size_class <- factor(exposure$input_size_class, levels = input_levels)
  positive <- exposure[is.finite(exposure$output_size_mm) & exposure$output_size_mm > 0 &
                         exposure$output_count > 0, , drop = FALSE]
  summary <- summarize_groups(exposure)
  summary$n_zero <- summary$n_individuals - summary$n_detected
  facet_labels <- ggplot2::labeller(input_size_class = function(x) paste0(x, " mm"))
  life_scale <- function() {
    ggplot2::scale_fill_manual(values = mp_life_form_colours, labels = life_labels,
                              limits = life_levels, drop = FALSE)
  }
  x_groups <- function() {
    ggplot2::scale_x_discrete(labels = life_labels, limits = life_levels, drop = FALSE)
  }
  group_theme <- function() {
    theme_mp(11) + ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 9)
    )
  }
  panel_pvalues <- function(outcome) {
    selected <- omnibus[omnibus$outcome == outcome, , drop = FALSE]
    selected$polymer <- factor(selected$polymer, levels = polymer_levels)
    selected$input_size_class <- factor(selected$input_size_class, levels = input_levels)
    selected$label <- mp_p_expression(selected$p_bh)
    selected$x_group <- life_levels[3]
    selected
  }
  add_panel_pvalues <- function(plot, outcome) {
    plot + ggplot2::geom_text(
      data = panel_pvalues(outcome),
      ggplot2::aes(x = x_group, y = Inf, label = label),
      inherit.aes = FALSE, parse = TRUE, vjust = 1.4, size = 3.15
    )
  }

  counts <- ggplot2::ggplot(exposure, ggplot2::aes(life_form, output_count, fill = life_form)) +
    ggplot2::geom_point(shape = 21, colour = "black", size = 2.1, stroke = 0.35,
      position = ggplot2::position_jitter(width = 0.16, height = 0, seed = 123)) +
    ggplot2::geom_point(data = summary, ggplot2::aes(y = count_median),
                        shape = 95, size = 8, colour = "black") +
    ggplot2::facet_grid(input_size_class ~ polymer, labeller = facet_labels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.18))) +
    x_groups() + life_scale() + group_theme() +
    ggplot2::labs(x = NULL, y = "Recovered particle count per individual")
  counts <- add_panel_pvalues(counts, "output_count")

  # A separate categorical strip retains zero-count individuals without
  # assigning a numerical particle size to a sample containing no particles.
  zero_strip <- function() {
    ggplot2::ggplot(summary, ggplot2::aes(life_form, input_size_class, fill = life_form)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.7, alpha = 0.23, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = n_zero), size = 3.5) +
      ggplot2::facet_grid(. ~ polymer) + x_groups() + life_scale() +
      ggplot2::scale_y_discrete(labels = function(x) paste0(x, " mm"),
                                limits = rev(input_levels), drop = FALSE) +
      group_theme() + ggplot2::labs(x = NULL, y = "No particles (n)") +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     panel.spacing = grid::unit(1.2, "lines"))
  }
  size_summary <- summary[is.finite(summary$size_median_mm), , drop = FALSE]
  size <- ggplot2::ggplot(positive, ggplot2::aes(life_form, output_size_mm, fill = life_form)) +
    ggplot2::geom_point(shape = 21, colour = "black", size = 2.1, stroke = 0.35,
      position = ggplot2::position_jitter(width = 0.16, height = 0, seed = 123)) +
    # Medians are computed on the original scale before the log transformation.
    ggplot2::geom_point(data = size_summary, ggplot2::aes(y = size_median_mm),
                        shape = 95, size = 8, colour = "black") +
    ggplot2::facet_grid(input_size_class ~ polymer, labeller = facet_labels) +
    ggplot2::scale_y_log10(expand = ggplot2::expansion(mult = c(0.06, 0.2))) +
    x_groups() + life_scale() + group_theme() +
    ggplot2::labs(x = NULL, y = "Mean particle size per individual (mm)") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
  size <- add_panel_pvalues(size, "output_size_mm")
  size <- patchwork::wrap_plots(size, zero_strip(), ncol = 1, heights = c(3.4, 1.45))

  controls_data <- data[data$is_control, , drop = FALSE]
  # Use deterministic horizontal offsets so all zero-valued controls are visible.
  controls_data$.x <- as.numeric(factor(controls_data$life_form, levels = life_levels))
  for (life in life_levels) {
    index <- which(as.character(controls_data$life_form) == life)
    offsets <- if (length(index) > 1L) seq(-0.22, 0.22, length.out = length(index)) else 0
    controls_data$.x[index] <- controls_data$.x[index] + offsets
  }
  controls <- ggplot2::ggplot(controls_data, ggplot2::aes(.x, output_count, fill = life_form)) +
    ggplot2::geom_point(shape = 21, size = 3, stroke = 0.5, colour = "black") +
    ggplot2::scale_x_continuous(breaks = seq_along(life_levels),
      labels = unname(life_labels[life_levels]), limits = c(0.5, 5.5)) +
    ggplot2::scale_y_continuous(breaks = 0, limits = c(-0.12, 0.12)) +
    life_scale() + theme_mp(12) + ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = NULL, y = "Recovered particle count per individual")

  scatter <- ggplot2::ggplot(positive, ggplot2::aes(output_count, output_size_mm, fill = life_form)) +
    ggplot2::geom_point(shape = 21, colour = "black", size = 2.5, alpha = 0.8, show.legend = TRUE) +
    ggplot2::facet_grid(input_size_class ~ polymer, labeller = facet_labels) +
    ggplot2::scale_y_log10() + life_scale() + theme_mp(11) +
    ggplot2::labs(x = "Recovered particle count per individual", y = "Mean particle size (mm)", fill = "Life form") +
    ggplot2::theme(legend.position = "bottom")
  scatter <- patchwork::wrap_plots(scatter, zero_strip(), ncol = 1,
                                   heights = c(3.4, 1.45), guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  scores <- ggplot2::ggplot(pca$scores, ggplot2::aes(PC1, PC2, colour = life_form, shape = input_size_class)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.35) +
    ggplot2::geom_point(size = 2.4, alpha = 0.8, show.legend = TRUE) +
    ggplot2::scale_colour_manual(values = mp_life_form_colours, labels = life_labels,
                                 limits = life_levels, drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(16, 17)) + ggplot2::coord_equal() + theme_mp(12) +
    ggplot2::labs(x = sprintf("PC1 (%.1f%%)", 100 * pca$variance$explained_fraction[1]),
                  y = sprintf("PC2 (%.1f%%)", 100 * pca$variance$explained_fraction[2]),
                  colour = "Life form", shape = "Input size (mm)") +
    ggplot2::theme(legend.position = "bottom")
  zeros_by_life <- summary |>
    dplyr::group_by(life_form, .drop = FALSE) |>
    dplyr::summarise(n_zero = sum(n_zero), .groups = "drop")
  pca_zeros <- ggplot2::ggplot(zeros_by_life, ggplot2::aes(n_zero, life_form, fill = life_form)) +
    ggplot2::geom_col(width = 0.65, alpha = 0.75, colour = "black", linewidth = 0.35, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = n_zero), hjust = -0.3, size = 3.5) +
    ggplot2::scale_y_discrete(labels = life_labels, limits = rev(life_levels), drop = FALSE) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    life_scale() + theme_mp(11) + ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "No particles (n)", y = NULL)
  scores <- patchwork::wrap_plots(scores, pca_zeros, nrow = 1,
                                  widths = c(2.1, 1), guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  detection <- ggplot2::ggplot(summary, ggplot2::aes(polymer, life_form, fill = detection_fraction)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(n_detected, "/", n_individuals)), size = 4) +
    ggplot2::facet_wrap(~ input_size_class, labeller = facet_labels) +
    ggplot2::scale_y_discrete(labels = life_labels, limits = rev(life_levels), drop = FALSE) +
    ggplot2::scale_fill_gradient(low = mp_heatmap_colours[["pale"]],
                                 high = mp_heatmap_colours[["azure"]], limits = c(0, 1)) + theme_mp(12) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Detected fraction")

  effects <- inference$contrasts[inference$contrasts$comparison_axis == "input_size", , drop = FALSE]
  effect_grid <- expand.grid(life_form = life_levels, polymer = polymer_levels, stringsAsFactors = FALSE)
  effect_plot <- function(outcome) {
    selected <- effects[effects$outcome == outcome, , drop = FALSE]
    selected <- merge(effect_grid, selected, by = c("life_form", "polymer"), all.x = TRUE, sort = FALSE)
    selected$life_form <- factor(selected$life_form, levels = rev(life_levels))
    selected$polymer <- factor(selected$polymer, levels = polymer_levels)
    selected$p_label <- mp_p_expression(selected$p_bh)
    size_outcome <- outcome == "output_size_mm"
    estimates <- selected[is.finite(selected$effect), , drop = FALSE]
    intervals <- selected[is.finite(selected$ci_lower) & is.finite(selected$ci_upper), , drop = FALSE]
    finite_x <- c(estimates$effect, intervals$ci_lower, intervals$ci_upper, if (size_outcome) 1 else 0)
    transformed <- if (size_outcome) log10(finite_x) else finite_x
    bounds <- range(transformed)
    span <- max(diff(bounds), if (size_outcome) 1 else 0.1)
    limits <- bounds + c(-0.08, 0.92) * span
    label_x <- bounds[2] + 0.9 * span
    if (size_outcome) {
      limits <- 10^limits
      label_x <- 10^label_x
    }
    selected$.p_x <- label_x
    plot <- ggplot2::ggplot(selected, ggplot2::aes(y = life_form)) +
      ggplot2::geom_vline(xintercept = if (size_outcome) 1 else 0, linetype = 2, colour = "grey50") +
      ggplot2::geom_errorbar(data = intervals,
        ggplot2::aes(xmin = ci_lower, xmax = ci_upper), orientation = "y", width = 0.18) +
      ggplot2::geom_point(data = estimates, ggplot2::aes(x = effect, fill = life_form),
                          shape = 21, size = 2.7, colour = "black") +
      ggplot2::geom_text(ggplot2::aes(x = .p_x, label = p_label), parse = TRUE,
                         hjust = 1, size = 3.05) +
      ggplot2::facet_wrap(~ polymer, nrow = 1, drop = FALSE) +
      ggplot2::scale_y_discrete(labels = life_labels, limits = rev(life_levels), drop = FALSE) +
      life_scale() + theme_mp(11) + ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = NULL,
        x = if (size_outcome) "Geometric mean size ratio (0.1-0.2 / 3-5 mm input)" else
          "Mean count difference (0.1-0.2 minus 3-5 mm input)")
    if (size_outcome) {
      breaks <- 10^seq(floor(bounds[1]), ceiling(bounds[2]))
      plot <- plot + ggplot2::scale_x_log10(limits = limits, breaks = breaks[breaks >= limits[1] & breaks <= max(finite_x) * 1.05])
    } else {
      breaks <- pretty(range(finite_x), n = 3)
      plot <- plot + ggplot2::scale_x_continuous(limits = limits,
        breaks = breaks[breaks >= limits[1] & breaks <= max(finite_x) + 0.05 * span])
    }
    plot
  }

  # Every planned pair appears, including undefined size comparisons as NA.
  pairwise_plot <- function(outcome, axis) {
    levels_to_compare <- if (axis == "life_form") life_levels else polymer_levels
    strata_levels <- if (axis == "life_form") polymer_levels else life_levels
    pairs <- utils::combn(levels_to_compare, 2, simplify = FALSE)
    pair_table <- do.call(rbind, lapply(pairs, function(pair) {
      data.frame(group_a = pair[1], group_b = pair[2], stringsAsFactors = FALSE)
    }))
    strata <- expand.grid(stratum = strata_levels, input_size_class = input_levels,
                           stringsAsFactors = FALSE)
    full <- merge(pair_table, strata, by = NULL)
    stratum_column <- if (axis == "life_form") "polymer" else "life_form"
    names(full)[names(full) == "stratum"] <- stratum_column
    selected <- inference$contrasts[inference$contrasts$outcome == outcome &
                                     inference$contrasts$comparison_axis == axis, , drop = FALSE]
    keys <- c("group_a", "group_b", stratum_column, "input_size_class")
    full <- merge(full, selected[c(keys, "p_bh")], by = keys, all.x = TRUE, sort = FALSE)
    pair_id <- function(a, b) paste(a, b, sep = " / ")
    pair_ids <- pair_id(pair_table$group_a, pair_table$group_b)
    pair_labels <- if (axis == "life_form") {
      pair_id(life_labels[pair_table$group_a], life_labels[pair_table$group_b])
    } else pair_ids
    full$pair <- factor(pair_id(full$group_a, full$group_b), levels = rev(pair_ids))
    full$stratum <- factor(full[[stratum_column]], levels = strata_levels)
    full$input_size_class <- factor(full$input_size_class, levels = input_levels)
    full$p_label <- mp_format_p(full$p_bh)
    plot <- ggplot2::ggplot(full, ggplot2::aes(stratum, pair, fill = p_bh)) +
      ggplot2::geom_tile(colour = "#E0E0E0", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = p_label), size = 3.6) +
      ggplot2::facet_wrap(~ input_size_class, nrow = 1, labeller = facet_labels) +
      ggplot2::scale_y_discrete(labels = stats::setNames(pair_labels, pair_ids), drop = FALSE) +
      ggplot2::scale_x_discrete(labels = if (axis == "polymer") life_labels else ggplot2::waiver(), drop = FALSE) +
      ggplot2::scale_fill_gradient(low = mp_heatmap_colours[["azure"]],
                                   high = mp_heatmap_colours[["pale"]], limits = c(0, 1),
                                   breaks = c(0, 0.5, 1), na.value = mp_heatmap_colours[["missing"]],
                                   name = expression(p[BH]),
                                   guide = ggplot2::guide_colourbar(barwidth = grid::unit(4, "cm"))) +
      theme_mp(11) + ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                     legend.position = "bottom", panel.spacing = grid::unit(1.5, "lines"))
    if (axis == "polymer") {
      plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
    plot
  }

  list(counts = counts, size = size, controls = controls, count_size = scatter,
       pca_scores = scores, detection = detection,
       size_effects = effect_plot("output_size_mm"), count_effects = effect_plot("output_count"),
       count_life_form_pvalues = pairwise_plot("output_count", "life_form"),
       size_life_form_pvalues = pairwise_plot("output_size_mm", "life_form"),
       count_polymer_pvalues = pairwise_plot("output_count", "polymer"),
       size_polymer_pvalues = pairwise_plot("output_size_mm", "polymer"))
}
