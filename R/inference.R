# Stratified comparisons for independent biological replicates.
# Permutation tests require exchangeability under the null distribution.
# Welch intervals are approximate, unadjusted intervals for individual contrasts.

mp_permutation_difference <- function(x, y, max_exact = 100000L,
                                      monte_carlo_B = 9999L) {
  stopifnot(length(x) >= 2L, length(y) >= 2L,
            all(is.finite(x)), all(is.finite(y)))
  pooled <- c(x, y)
  n_a <- length(x)
  n_b <- length(y)
  observed <- mean(x) - mean(y)
  tolerance <- 100 * .Machine$double.eps * max(1, abs(observed))
  log_permutations <- lchoose(n_a + n_b, n_a)
  if (log_permutations <= log(max_exact) + 1e-10) {
    permutations <- utils::combn(seq_along(pooled), n_a)
    means_a <- colMeans(matrix(pooled[permutations], nrow = n_a))
    differences <- means_a - (sum(pooled) - n_a * means_a) / n_b
    p <- mean(abs(differences) >= abs(observed) - tolerance)
    method <- "Exact two-sided label permutation"
    n_permutations <- length(differences)
  } else {
    differences <- replicate(monte_carlo_B, {
      selected <- sample.int(length(pooled), size = n_a, replace = FALSE)
      mean(pooled[selected]) - mean(pooled[-selected])
    })
    p <- (1 + sum(abs(differences) >= abs(observed) - tolerance)) /
      (monte_carlo_B + 1)
    method <- "Monte Carlo two-sided label permutation (+1 correction)"
    n_permutations <- monte_carlo_B
  }
  list(difference = observed, p_value = p, method = method,
       n_permutations = n_permutations,
       n_possible_permutations = exp(log_permutations))
}

mp_welch_interval <- function(x, y, confidence = 0.95) {
  variance_a <- stats::var(x) / length(x)
  variance_b <- stats::var(y) / length(y)
  se_squared <- variance_a + variance_b
  if (!is.finite(se_squared) || se_squared <= 0) {
    return(list(lower = NA_real_, upper = NA_real_, df = NA_real_,
                status = "Unavailable: both groups are constant"))
  }
  df <- se_squared^2 /
    (variance_a^2 / (length(x) - 1) + variance_b^2 / (length(y) - 1))
  margin <- stats::qt((1 + confidence) / 2, df = df) * sqrt(se_squared)
  estimate <- mean(x) - mean(y)
  list(lower = estimate - margin, upper = estimate + margin, df = df,
       status = "Approximate; unadjusted; small-sample sensitivity")
}

mp_observed_levels <- function(x) {
  values <- unique(as.character(x[!is.na(x)]))
  if (is.factor(x)) intersect(levels(x), values) else sort(values)
}

mp_permutation_omnibus <- function(values, groups, monte_carlo_B = 9999L) {
  stopifnot(is.numeric(values), length(values) == length(groups),
            all(is.finite(values)), !anyNA(groups),
            length(monte_carlo_B) == 1L, is.finite(monte_carlo_B),
            monte_carlo_B >= 1L,
            monte_carlo_B == as.integer(monte_carlo_B))
  group_indices <- split(seq_along(values), droplevels(factor(groups)))
  group_n <- lengths(group_indices)
  stopifnot(length(group_n) >= 2L, all(group_n >= 2L))
  centered <- values - mean(values)
  # With fixed group sizes and pooled values, this statistic orders permutations
  # exactly as the one-way ANOVA F statistic, without dividing by zero variance.
  statistic <- function(x) {
    group_sums <- vapply(group_indices, function(i) sum(x[i]), numeric(1))
    sum(group_sums^2 / group_n)
  }
  observed <- statistic(centered)
  if (all(values == values[1])) {
    return(list(test_statistic = observed, p_value = 1,
                method = "All observations constant; exact p = 1",
                B = 0L))
  }
  tolerance <- 100 * .Machine$double.eps * max(1, abs(observed))
  permuted <- replicate(monte_carlo_B, {
    statistic(centered[sample.int(length(centered), replace = FALSE)])
  })
  list(test_statistic = observed,
       p_value = (1 + sum(permuted >= observed - tolerance)) /
         (monte_carlo_B + 1),
       method = "Monte Carlo omnibus label permutation (+1 correction)",
       B = monte_carlo_B)
}

run_omnibus <- function(data, config = list()) {
  # These panel-level tests complement, rather than replace, pairwise contrasts.
  input_column <- if (is.null(config$input_size_column)) {
    "input_size_class"
  } else config$input_size_column
  required <- c("life_form", "polymer", input_column,
                "output_count", "output_size_mm")
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Omnibus data are missing columns: ",
         paste(missing_columns, collapse = ", "))
  }
  if (!is.numeric(data$output_count) || !is.numeric(data$output_size_mm)) {
    stop("Response columns must be numeric.")
  }
  seed <- if (is.null(config$seed)) 20261025L else config$seed
  monte_carlo_B <- if (is.null(config$monte_carlo_B)) {
    9999L
  } else config$monte_carlo_B
  minimum_n <- if (is.null(config$min_group_n)) 2L else config$min_group_n
  stopifnot(length(seed) == 1L, is.finite(seed),
            length(monte_carlo_B) == 1L, is.finite(monte_carlo_B),
            monte_carlo_B >= 1L, monte_carlo_B == as.integer(monte_carlo_B),
            length(minimum_n) == 1L, is.finite(minimum_n),
            minimum_n >= 2L, minimum_n == as.integer(minimum_n))

  # Restore the caller's random-number state after reproducible permutations.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  data$.input_size_class <- data[[input_column]]
  is_control <- tolower(as.character(data$polymer)) == "control"
  complete_metadata <- stats::complete.cases(
    data[c("life_form", "polymer", ".input_size_class")])
  data <- data[complete_metadata & !is.na(is_control) & !is_control, , drop = FALSE]
  strata <- unique(data[c("polymer", ".input_size_class")])
  rows <- list()
  for (outcome in c("output_count", "output_size_mm")) {
    for (stratum_index in seq_len(nrow(strata))) {
      stratum <- strata[stratum_index, , drop = FALSE]
      selected <- as.character(data$polymer) == as.character(stratum$polymer) &
        as.character(data$.input_size_class) ==
        as.character(stratum$.input_size_class)
      subset_data <- data[selected, , drop = FALSE]
      values <- subset_data[[outcome]]
      if (outcome == "output_size_mm") {
        # Particle size is conditional on recovery and is undefined at zero count.
        eligible <- is.finite(values) & values > 0 &
          is.finite(subset_data$output_count) & subset_data$output_count > 0
      } else {
        eligible <- is.finite(values) & values >= 0
      }
      eligible_groups <- table(as.character(subset_data$life_form[eligible]))
      retained_groups <- names(eligible_groups)[eligible_groups >= minimum_n]
      eligible <- eligible & as.character(subset_data$life_form) %in% retained_groups
      values <- values[eligible]
      if (outcome == "output_size_mm") values <- log(values)
      groups <- as.character(subset_data$life_form[eligible])
      row <- data.frame(
        outcome = outcome, comparison_axis = "life_form",
        family = paste(outcome, "life_form_omnibus", sep = "__"),
        polymer = as.character(stratum$polymer),
        input_size_class = as.character(stratum$.input_size_class),
        n = length(values), n_source = nrow(subset_data),
        n_groups = length(retained_groups),
        groups = paste(retained_groups, collapse = "; "),
        test_statistic = NA_real_,
        test_scale = if (outcome == "output_size_mm") {
          "Between-group sum of squares of natural log mean size"
        } else "Between-group sum of squares of count",
        p_value = NA_real_, p_bh = NA_real_, method = NA_character_,
        B = 0L, seed = seed, status = "Not tested: fewer than two eligible groups",
        assumption = "Independent biological replicates; exchangeability under the null",
        stringsAsFactors = FALSE
      )
      if (length(retained_groups) >= 2L) {
        test <- mp_permutation_omnibus(values, groups, monte_carlo_B)
        row$test_statistic <- test$test_statistic
        row$p_value <- test$p_value
        row$method <- test$method
        row$B <- test$B
        row$status <- "Tested"
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  result <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (nrow(result)) {
    for (family_name in unique(result$family)) {
      family_rows <- result$family == family_name
      tested_rows <- family_rows & is.finite(result$p_value)
      # The family contains every planned stratum, including an untestable one.
      result$p_bh[tested_rows] <- stats::p.adjust(
        result$p_value[tested_rows], method = "BH", n = sum(family_rows))
    }
    rownames(result) <- NULL
  }
  result
}

run_inference <- function(data, config = list()) {
  # Input-size classes define strata, rather than a numeric continuous dose.
  input_column <- if (is.null(config$input_size_column)) {
    "input_size_class"
  } else config$input_size_column
  required <- c("life_form", "polymer", input_column,
                "output_count", "output_size_mm")
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Inference data are missing columns: ",
         paste(missing_columns, collapse = ", "))
  }
  data$.input_size_class <- data[[input_column]]
  seed <- if (is.null(config$seed)) 20261025L else config$seed
  max_exact <- if (is.null(config$max_exact_permutations)) {
    100000L
  } else config$max_exact_permutations
  monte_carlo_B <- if (is.null(config$monte_carlo_B)) {
    9999L
  } else config$monte_carlo_B
  minimum_n <- if (is.null(config$min_group_n)) 2L else config$min_group_n
  stopifnot(length(seed) == 1L, is.finite(seed),
            length(max_exact) == 1L, is.finite(max_exact), max_exact >= 1,
            length(monte_carlo_B) == 1L, is.finite(monte_carlo_B),
            monte_carlo_B >= 1, monte_carlo_B == as.integer(monte_carlo_B),
            length(minimum_n) == 1L, is.finite(minimum_n), minimum_n >= 2)

  # Preserve the caller's random-number state after reproducible simulations.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)

  # Controls have no experimental input-size class and are assessed separately.
  is_control <- tolower(as.character(data$polymer)) == "control"
  valid_metadata <- stats::complete.cases(
    data[c("life_form", "polymer", ".input_size_class")])
  data <- data[valid_metadata & !is.na(is_control) & !is_control, , drop = FALSE]
  if (!is.numeric(data$output_count) || !is.numeric(data$output_size_mm)) {
    stop("Response columns must be numeric.")
  }

  axes <- list(
    life_form = list(variable = "life_form", by = c("polymer", ".input_size_class")),
    polymer = list(variable = "polymer", by = c("life_form", ".input_size_class")),
    input_size = list(variable = ".input_size_class", by = c("life_form", "polymer"))
  )
  contrast_rows <- list()
  skipped_rows <- list()
  for (outcome in c("output_count", "output_size_mm")) {
    for (axis_name in names(axes)) {
      axis <- axes[[axis_name]]
      strata <- unique(data[axis$by])
      if (!nrow(strata)) next
      for (stratum_index in seq_len(nrow(strata))) {
        stratum <- strata[stratum_index, , drop = FALSE]
        selected <- rep(TRUE, nrow(data))
        for (column in axis$by) {
          selected <- selected & as.character(data[[column]]) ==
            as.character(stratum[[column]])
        }
        subset_data <- data[selected, , drop = FALSE]
        levels_here <- mp_observed_levels(subset_data[[axis$variable]])
        if (length(levels_here) < 2L) next
        pairs <- utils::combn(levels_here, 2L, simplify = FALSE)
        for (pair in pairs) {
          a <- subset_data[as.character(subset_data[[axis$variable]]) == pair[1],
                           , drop = FALSE]
          b <- subset_data[as.character(subset_data[[axis$variable]]) == pair[2],
                           , drop = FALSE]
          x <- a[[outcome]]
          y <- b[[outcome]]
          if (outcome == "output_size_mm") {
            # A mean particle size is undefined in samples with no particles.
            x <- log(x[is.finite(x) & x > 0 &
                         is.finite(a$output_count) & a$output_count > 0])
            y <- log(y[is.finite(y) & y > 0 &
                         is.finite(b$output_count) & b$output_count > 0])
          } else {
            x <- x[is.finite(x) & x >= 0]
            y <- y[is.finite(y) & y >= 0]
          }
          row <- data.frame(
            outcome = outcome, comparison_axis = axis_name,
            family = paste(outcome, axis_name, sep = "__"),
            life_form = if ("life_form" %in% axis$by)
              as.character(stratum$life_form) else NA_character_,
            polymer = if ("polymer" %in% axis$by)
              as.character(stratum$polymer) else NA_character_,
            input_size_class = if (".input_size_class" %in% axis$by)
              as.character(stratum$.input_size_class) else NA_character_,
            group_a = pair[1], group_b = pair[2], n_a = length(x), n_b = length(y),
            n_source_a = nrow(a), n_source_b = nrow(b),
            stringsAsFactors = FALSE
          )
          if (length(x) < minimum_n || length(y) < minimum_n) {
            row$reason <- paste0("Fewer than ", minimum_n,
                                 " finite eligible observations in at least one group")
            skipped_rows[[length(skipped_rows) + 1L]] <- row
            next
          }
          test <- mp_permutation_difference(x, y, max_exact, monte_carlo_B)
          interval <- mp_welch_interval(x, y)
          size_outcome <- outcome == "output_size_mm"
          row$mean_a <- if (size_outcome) exp(mean(x)) else mean(x)
          row$mean_b <- if (size_outcome) exp(mean(y)) else mean(y)
          row$mean_type <- if (size_outcome) "Geometric mean of individual mean sizes" else
            "Arithmetic mean count per individual"
          row$effect <- if (size_outcome) exp(test$difference) else test$difference
          row$effect_type <- if (size_outcome) "Geometric mean ratio A / B" else
            "Mean count difference A - B"
          row$test_statistic <- test$difference
          row$test_scale <- if (size_outcome) "Natural log of mean size" else "Count"
          row$ci_lower <- if (size_outcome) exp(interval$lower) else interval$lower
          row$ci_upper <- if (size_outcome) exp(interval$upper) else interval$upper
          row$ci_method <- "Unadjusted approximate 95% Welch t interval"
          row$ci_status <- interval$status
          row$ci_df <- interval$df
          row$p_value <- test$p_value
          row$permutation_method <- test$method
          row$n_permutations <- test$n_permutations
          row$n_possible_permutations <- test$n_possible_permutations
          row$assumption <- "Independent biological replicates; exchangeability under the null"
          contrast_rows[[length(contrast_rows) + 1L]] <- row
        }
      }
    }
  }

  contrasts <- if (length(contrast_rows)) do.call(rbind, contrast_rows) else data.frame()
  skipped <- if (length(skipped_rows)) do.call(rbind, skipped_rows) else data.frame()
  family_summary <- data.frame()
  if (nrow(contrasts)) {
    # Benjamini-Hochberg adjustment covers each outcome-by-axis family.
    contrasts$p_bh <- NA_real_
    for (family_name in unique(contrasts$family)) {
      selected <- contrasts$family == family_name
      contrasts$p_bh[selected] <- stats::p.adjust(contrasts$p_value[selected], "BH")
      current <- contrasts[selected, , drop = FALSE]
      family_summary <- rbind(family_summary, data.frame(
        family = family_name, outcome = current$outcome[1],
        comparison_axis = current$comparison_axis[1],
        n_tested = nrow(current),
        n_skipped = if (nrow(skipped)) sum(skipped$family == family_name) else 0L,
        n_bh_below_0_05 = sum(current$p_bh < 0.05),
        smallest_raw_p = min(current$p_value),
        stringsAsFactors = FALSE
      ))
    }
    rownames(contrasts) <- NULL
    rownames(family_summary) <- NULL
  }
  if (nrow(skipped)) rownames(skipped) <- NULL
  list(contrasts = contrasts, family_summary = family_summary, skipped = skipped)
}
