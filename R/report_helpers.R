# Display executable source blocks without long comment sections.
report_code <- function(path, objects = NULL) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!is.null(objects)) {
    expressions <- parse(text = lines, keep.source = TRUE)
    object_names <- vapply(expressions, function(x) {
      if (is.call(x) && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]])) {
        as.character(x[[2]])
      } else ""
    }, character(1))
    if (!all(objects %in% object_names)) stop("Missing source object in ", path)
    references <- attr(expressions, "srcref")[object_names %in% objects]
    lines <- unlist(lapply(references, function(x) lines[seq.int(x[1], x[3])]),
                    use.names = FALSE)
  }
  lines <- lines[!grepl("^[[:space:]]*#", lines)]
  if (length(lines) > 1L) {
    blank <- !nzchar(trimws(lines))
    lines <- lines[!(blank & c(FALSE, head(blank, -1L)))]
  }
  # Unicode escapes preserve source filenames while keeping code text in English.
  vapply(lines, function(line) {
    characters <- utf8ToInt(line)
    paste(vapply(characters, function(x) {
      if (x < 128L) intToUtf8(x) else if (x <= 65535L) {
        sprintf("\\u%04x", x)
      } else sprintf("\\U%08x", x)
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

show_table <- function(x, digits = 3) {
  p_columns <- names(x)[grepl("(^p(_|$)|_p$)", names(x))]
  for (column in p_columns) {
    if (is.numeric(x[[column]])) x[[column]] <- vapply(x[[column]], function(value) {
      if (!is.finite(value)) return("NA")
      if (value == 0) return("< 2.23e-308")
      format(value, digits = 3, scientific = value < 0.001, trim = TRUE)
    }, character(1))
  }
  cat('<div class="table-scroll">')
  print(knitr::kable(x, format = "html", digits = digits, row.names = FALSE,
                    escape = TRUE))
  cat('</div>')
}
