# Plot styling inspired by the supplied bat gut analysis figures.
# Use raw observations, black point outlines, and clearly labelled summaries.

mp_reference_colours <- c(
  blue = "#0000FF", red = "#FF0000", grey = "#BEBEBE",
  teal = "#1B9E9A", ochre = "#DCA65E", purple = "#96279B"
)

# Assign stable colours to explicitly ordered group levels.
# Pass control = "<level>" to reserve grey for a reference group.
mp_palette <- function(levels, control = NULL) {
  levels <- unique(as.character(levels))
  if (length(levels) > length(mp_reference_colours)) {
    stop("The reference palette supports at most six groups.")
  }
  if (!is.null(control) && (length(control) != 1L || !control %in% levels)) {
    stop("The control must be exactly one of the supplied group levels.")
  }
  if (is.null(control)) {
    return(stats::setNames(unname(mp_reference_colours)[seq_along(levels)], levels))
  }
  result <- stats::setNames(rep(NA_character_, length(levels)), levels)
  result[control] <- mp_reference_colours[["grey"]]
  other_levels <- setdiff(levels, control)
  other_colours <- mp_reference_colours[names(mp_reference_colours) != "grey"]
  result[other_levels] <- unname(other_colours)[seq_along(other_levels)]
  result
}

# Keep life-form colours identical across endpoint plots and PCA panels.
mp_life_form_colours <- mp_palette(c(
  "small_juloid", "middle_juloid", "big_juloid", "polydesmoid", "platydesmoid"
))

# Keep continuous heatmaps in azure shades while retaining black numeric labels.
mp_heatmap_colours <- c(pale = "#EAF7FF", azure = "#4DB8EA", missing = "#E3E3E3")

# Match the reference figures' uncluttered white panels and black axes.
theme_mp <- function(base_size = 12, base_family = "sans", grid = FALSE) {
  result <- ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(size = base_size + 1),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.5),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length = grid::unit(1.5, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = "black"),
      panel.spacing = grid::unit(1.2, "lines"),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title = ggplot2::element_text(face = "plain", size = base_size + 2, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, hjust = 0),
      plot.caption = ggplot2::element_text(size = base_size - 2, hjust = 0),
      plot.tag = ggplot2::element_text(face = "plain", size = base_size + 4),
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
  if (isTRUE(grid)) {
    result <- result + ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#ECECEC", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank()
    )
  }
  result
}

# Export both a high-resolution raster image and a scalable PDF.
# The filename is a stem without an extension; dimensions are in inches.
save_mp_plot <- function(plot, filename, width = 9, height = 6, dpi = 600) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = paste0(filename, ".png"), plot = plot,
    width = width, height = height, units = "in", dpi = dpi, bg = "white"
  )
  pdf_device <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(
    filename = paste0(filename, ".pdf"), plot = plot,
    width = width, height = height, units = "in", device = pdf_device, bg = "white"
  )
  invisible(c(png = paste0(filename, ".png"), pdf = paste0(filename, ".pdf")))
}
