# Check scientific invariants and exact inference, not cosmetic implementation details.
# Run from the repository root after scripts/run_analysis.R.
source("R/data.R")
source("R/inference.R")
source("R/analysis.R")
source("R/figures.R")
source("config.R")
d <- input_data
stopifnot(nrow(d) == 275L, !anyNA(d$sample_id), !anyDuplicated(d$sample_id),
          sum(!d$is_control) == 250L, sum(d$is_control) == 25L,
          all(is.finite(d$output_count)), all(d$output_count >= 0),
          all(d$output_count == floor(d$output_count)),
          all(is.finite(d$output_size_raw)), all(d$output_size_raw >= 0),
          all((d$output_size_raw == 0) == (d$output_count == 0)),
          identical(is.na(d$output_size_mm), d$output_count == 0),
          all(d$output_size_mm[d$output_count > 0] > 0),
          all(d$output_size_mm[d$output_count > 0] == d$output_size_raw[d$output_count > 0]),
          identical(is.na(d$input_size_class), d$is_control),
          identical(is.na(d$input_lower_mm), d$is_control),
          identical(is.na(d$input_upper_mm), d$is_control),
          all(d$is_control == (d$polymer == "control")),
          setequal(d$life_form, life_levels),
          all(table(d$life_form[d$is_control]) == 5L),
          all(d$output_count[d$is_control] == 0L),
          all(d$output_size_raw[d$is_control] == 0))
exposure <- d[!d$is_control, ]
stopifnot(setequal(exposure$polymer, polymer_levels),
          setequal(exposure$input_size_class, c("0.1-0.2", "3-5")),
          all(table(exposure$life_form, exposure$polymer, exposure$input_size_class) == 5L),
          sum(exposure$output_count == 0L) == 57L,
          all(exposure$output_count[exposure$life_form == "platydesmoid"] == 0L),
          all(exposure$input_lower_mm == ifelse(exposure$input_size_class == "0.1-0.2", 0.1, 3)),
          all(exposure$input_upper_mm == ifelse(exposure$input_size_class == "0.1-0.2", 0.2, 5)))
exposure_groups <- interaction(exposure$life_form, exposure$polymer, exposure$input_size_class,
                               drop = TRUE)
for (labels in c(split(exposure$replicate_label, exposure_groups),
                 split(d$replicate_label[d$is_control], d$life_form[d$is_control]))) {
  stopifnot(length(labels) == 5L, setequal(labels, 1:5))
}

# Independent enumeration benchmark: fully separated 5-versus-5 groups.
t <- mp_permutation_difference(1:5, 6:10)
stopifnot(abs(t$p_value - 2 / choose(10, 5)) < 1e-12,
          t$n_permutations == 252L,
          mp_permutation_difference(rep(0, 5), rep(0, 5))$p_value == 1,
          abs(t$p_value - mp_permutation_difference(6:10, 1:5)$p_value) < 1e-12)
interval <- mp_welch_interval(c(1, 2, 4, 5, 8), c(2, 3, 7, 9, 12))
benchmark <- t.test(c(1, 2, 4, 5, 8), c(2, 3, 7, 9, 12))$conf.int
stopifnot(isTRUE(all.equal(c(interval$lower, interval$upper), as.numeric(benchmark))))

r <- readRDS("results/analysis_results.rds")
stopifnot(identical(r$data$sample_id, d$sample_id),
          identical(r$primary$sample_id, exposure$sample_id))
for (column in names(d)) {
  observed <- r$data[[column]]
  expected <- d[[column]]
  if (is.factor(observed)) observed <- as.character(observed)
  if (is.factor(expected)) expected <- as.character(expected)
  stopifnot(identical(observed, expected))
}
c <- r$inference$contrasts
stopifnot("p_bh" %in% names(c), "p_bh" %in% names(r$omnibus))
for (family in unique(c$family)) {
  use <- c$family == family
  stopifnot(isTRUE(all.equal(c$p_bh[use], p.adjust(c$p_value[use], "BH"))))
}
stopifnot(all(c$p_value > 0 & c$p_value <= 1), all(c$p_bh >= c$p_value - 1e-12),
          all(c$p_bh <= 1),
          abs(sum(r$pca$variance$explained_fraction) - 1) < 1e-12,
          nrow(r$pca$scores) == 193L,
          nrow(r$primary) == 250L,
          sum(r$primary$output_count == 0L) == 57L,
          sum(r$primary$life_form == "platydesmoid") == 50L,
          r$primary$output_count[r$primary$sample_id == "sj pvc 01-02 5"] == 0L,
          identical(r$pca$scores$sample_id, exposure$sample_id[exposure$output_count > 0]))
stopifnot(all(r$omnibus$n[r$omnibus$outcome == "output_count"] == 25L),
          all(r$omnibus$n_groups[r$omnibus$outcome == "output_count"] == 5L),
          all(r$omnibus$n_groups[r$omnibus$outcome == "output_size_mm"] == 4L))
for (outcome in unique(r$omnibus$outcome)) {
  use <- r$omnibus$outcome == outcome
  stopifnot(isTRUE(all.equal(r$omnibus$p_bh[use], p.adjust(r$omnibus$p_value[use], "BH"))))
}
zero_contrasts <- c[c$outcome == "output_count" & c$comparison_axis == "input_size" & c$life_form == "platydesmoid", ]
stopifnot(nrow(zero_contrasts) == 5L, all(zero_contrasts$effect == 0),
          all(zero_contrasts$p_value == 1), all(zero_contrasts$p_bh == 1))

# All five controls per life form must remain individually visible at zero.
drawn_controls <- ggplot2::ggplot_build(r$plots$controls)$data[[1]]
stopifnot(nrow(drawn_controls) == 25L, all(drawn_controls$y == 0),
          !anyDuplicated(drawn_controls$x))

# Validate the omnibus statistic against a closed-form two-group calculation.
omnibus_values <- c(1, 3, 2, 8, 9, 7)
omnibus_groups <- rep(c("A", "B"), each = 3L)
set.seed(42)
omnibus_check <- mp_permutation_omnibus(omnibus_values, omnibus_groups, monte_carlo_B = 99L)
group_a <- omnibus_values[omnibus_groups == "A"]
group_b <- omnibus_values[omnibus_groups == "B"]
expected_ss <- length(group_a) * length(group_b) / length(omnibus_values) *
  (mean(group_a) - mean(group_b))^2
stopifnot(abs(omnibus_check$test_statistic - expected_ss) < 1e-12,
          omnibus_check$p_value >= 1 / 100,
          mp_permutation_omnibus(rep(0, 6), omnibus_groups, 99L)$p_value == 1)

# A log axis must not change the arithmetic midpoint used by an even-sample median.
plot_sizes <- r$data[!r$data$is_control & !is.na(r$data$output_size_mm), ]
expected_medians <- aggregate(output_size_mm ~ life_form + polymer + input_size_class,
                              data = plot_sizes, FUN = median)$output_size_mm
drawn_medians <- ggplot2::ggplot_build(r$plots$size[[1]])$data[[2]]$y
stopifnot(isTRUE(all.equal(sort(drawn_medians[is.finite(drawn_medians)]),
                          sort(log10(expected_medians)), check.attributes = FALSE)))

# Verify that each displayed p-value belongs to the correct experimental panel.
check_panel_pvalues <- function(plot, outcome) {
  built <- ggplot2::ggplot_build(plot)
  displayed <- merge(built$data[[3]][c("PANEL", "label")],
    built$layout$layout[c("PANEL", "polymer", "input_size_class")], by = "PANEL")
  expected <- r$omnibus[r$omnibus$outcome == outcome, ]
  expected$label <- mp_p_expression(expected$p_bh)
  compared <- merge(displayed, expected[c("polymer", "input_size_class", "label")],
                    by = c("polymer", "input_size_class"), suffixes = c("_plot", "_test"))
  stopifnot(nrow(compared) == 10L, all(compared$label_plot == compared$label_test))
}
check_panel_pvalues(r$plots$counts, "output_count")
check_panel_pvalues(r$plots$size[[1]], "output_size_mm")

message("Passed: embedded-data integrity, experimental design, zero semantics, exact tests, Welch intervals, family correction and PCA.")
