#' Read a sheet from an '.xlsb' workbook
#'
#' Reads a single worksheet from an Excel binary (`.xlsb`) workbook into a
#' tibble. This is the `.xlsb` counterpart to [readxl::read_xlsx()].
#'
#' Cell types are taken from python-calamine: text becomes `character`, numbers
#' become `double`, and dates become [Date]. A column whose non-empty cells are
#' all of one type gets that type; a column with mixed types falls back to
#' `character`. Empty cells (and any string in `na`) become `NA`. Use
#' `col_types` to override this per column.
#'
#' The Python package `python-calamine` is installed automatically (via
#' `reticulate`) the first time you read a file.
#'
#' @param path Path to the `.xlsb` file.
#' @param sheet Sheet to read. Either a sheet name (string) or a positive
#'   integer giving its position in the workbook. Defaults to the first sheet.
#' @param col_names `TRUE` to use the first row as column names, `FALSE` to
#'   generate names (`...1`, `...2`, ...), or a character vector of names to use.
#' @param col_types `NULL` to guess the type of each column (the default), or a
#'   character vector of `"guess"`, `"text"`, `"numeric"`, `"date"`,
#'   `"logical"`, or `"skip"`. Recycled if length 1; otherwise must have one
#'   entry per column. `"skip"` drops the column from the result.
#' @param na Character vector of strings to treat as missing values, in
#'   addition to empty cells. Defaults to `""`.
#' @param skip Number of rows to skip before reading anything, be it column
#'   names or data. Rows are counted from the top of the sheet, and leading
#'   empty rows are always skipped automatically, so this is a lower bound.
#'   Defaults to 0.
#'
#' @return A [tibble][tibble::tibble] with one column per column of the sheet.
#'
#' @note
#' Cells are already parsed by python-calamine before `col_types` is applied,
#' so `"text"` is only fully faithful for cells Excel stored as text. For
#' cells stored as numbers, the original representation is already lost by the
#' time R sees them: dates coerced to `"text"` become an ISO string such as
#' `"2020-01-01"` (not the Excel serial number `readxl` returns, and not the
#' original display string), and a long numeric ID or code has already lost
#' any precision beyond what a double can hold.
#'
#' @seealso [list_sheets()] to list the sheets in a workbook.
#'
#' @examples
#' \dontrun{
#' rxlsb("workbook.xlsb")
#' rxlsb("workbook.xlsb", sheet = "data")
#' rxlsb("workbook.xlsb", col_names = FALSE)
#' rxlsb("workbook.xlsb", col_types = "text")
#' rxlsb("workbook.xlsb", skip = 2)
#' }
#'
#' @export
rxlsb <- function(
  path,
  sheet = 1,
  col_names = TRUE,
  col_types = NULL,
  na = "",
  skip = 0
) {
  skip <- .check_skip(skip)
  wb <- .workbook(path)
  sheet_name <- .resolve_sheet(wb, sheet)

  ws <- wb$get_sheet_by_name(sheet_name)
  rows <- ws$to_python()
  if (length(rows) == 0L) {
    return(tibble::tibble())
  }

  rows <- .skip_rows(rows, skip, first_row = as.integer(ws$start[[1L]]))
  if (length(rows) == 0L) {
    return(tibble::tibble())
  }

  # Split header row from data rows according to col_names.
  if (isTRUE(col_names)) {
    header <- as.character(unlist(rows[[1L]]))
    rows <- rows[-1L]
  } else if (is.character(col_names)) {
    header <- col_names
  } else {
    header <- paste0("...", seq_along(rows[[1L]]))
  }

  n_col <- length(header)
  types <- .resolve_col_types(col_types, n_col)

  cols <- lapply(seq_len(n_col), function(j) {
    vals <- lapply(rows, function(r) if (j <= length(r)) r[[j]] else NA)
    .build_column(vals, na, types[[j]])
  })
  names(cols) <- make.unique(header)

  keep <- types != "skip"
  cols <- cols[keep]
  if (length(cols) == 0L) {
    return(tibble::tibble())
  }

  tibble::as_tibble(cols, .name_repair = "minimal")
}

# Validate skip: a single non-negative whole number. Kept as double so very
# large values pass through without integer overflow.
.check_skip <- function(skip) {
  ok <- is.numeric(skip) &&
    length(skip) == 1L &&
    !is.na(skip) &&
    skip >= 0 &&
    skip == trunc(skip)
  if (!ok) {
    stop("`skip` must be a single non-negative whole number.", call. = FALSE)
  }
  as.numeric(skip)
}

# Apply readxl's skip semantics. `skip` counts absolute sheet rows, but
# `to_python()` has already trimmed the sheet's leading empty area, so
# rows[[1]] sits at 0-based sheet row `first_row` (from CalamineSheet$start)
# and only the part of skip reaching past it is applied here. Like readxl,
# any blank rows left at the top are then also dropped, which is what makes
# skip a lower bound rather than an exact count.
.skip_rows <- function(rows, skip, first_row) {
  drop <- as.integer(min(max(0, skip - first_row), length(rows)))
  if (drop > 0L) {
    rows <- rows[-seq_len(drop)]
  }
  while (length(rows) > 0L && .blank_row(rows[[1L]])) {
    rows <- rows[-1L]
  }
  rows
}

# A row is blank when every cell is empty as stored (length-0 or ""); the
# user's `na` strings are deliberately not consulted, matching readxl.
.blank_row <- function(row) {
  all(vapply(
    row,
    function(v) length(v) == 0L || (is.character(v) && !nzchar(v)),
    logical(1)
  ))
}

# Valid values for col_types, in the order they're documented.
.col_type_values <- c("guess", "text", "numeric", "date", "logical", "skip")

# Resolve a user-supplied col_types (NULL, or a length-1/length-n_col
# character vector) into a length-n_col character vector, validating as we go.
.resolve_col_types <- function(col_types, n_col) {
  if (is.null(col_types)) {
    return(rep("guess", n_col))
  }
  if (!is.character(col_types)) {
    stop("`col_types` must be NULL or a character vector.", call. = FALSE)
  }
  bad <- setdiff(col_types, .col_type_values)
  if (length(bad)) {
    stop(
      "`col_types` must be one of: ",
      paste(.col_type_values, collapse = ", "),
      ". Got: ",
      paste(unique(bad), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (length(col_types) == 1L) {
    return(rep(col_types, n_col))
  }
  if (length(col_types) != n_col) {
    stop(
      "`col_types` must have length 1 or ",
      n_col,
      " (one per column), not ",
      length(col_types),
      ".",
      call. = FALSE
    )
  }
  col_types
}

# Map a user-supplied sheet (name or 1-based index) to a sheet name.
.resolve_sheet <- function(wb, sheet) {
  names <- as.character(wb$sheet_names)
  if (is.character(sheet)) {
    if (length(sheet) != 1L || !sheet %in% names) {
      stop(
        "Sheet '",
        sheet,
        "' not found. Available sheets: ",
        paste(names, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    return(sheet)
  }
  if (is.numeric(sheet) && length(sheet) == 1L) {
    if (sheet < 1 || sheet > length(names)) {
      stop(
        "Sheet index ",
        sheet,
        " is out of range (workbook has ",
        length(names),
        " sheet(s)).",
        call. = FALSE
      )
    }
    return(names[[as.integer(sheet)]])
  }
  stop(
    "`sheet` must be a single sheet name or positive integer.",
    call. = FALSE
  )
}

# Turn a list of already-typed cells (one column) into a typed atomic vector.
# vapply with a fixed FUN.VALUE keeps each path type-stable; the Date class is
# set explicitly rather than relying on c() coercion. `type` is one of
# "guess", "text", "numeric", "date", "logical" ("skip" never reaches here;
# the caller drops those columns before building them).
.build_column <- function(vals, na, type = "guess") {
  empty <- vapply(
    vals,
    function(v) {
      length(v) == 0L ||
        (is.character(v) && length(v) == 1L && v %in% na)
    },
    logical(1)
  )

  switch(
    type,
    text = .build_column_text(vals, empty),
    numeric = .build_column_numeric(vals, empty),
    logical = .build_column_logical(vals, empty),
    date = .build_column_date(vals, empty),
    .build_column_guess(vals, empty)
  )
}

# Default ("guess") behavior: a column whose non-empty cells are all one type
# keeps that type; mixed types fall back to character.
.build_column_guess <- function(vals, empty) {
  target <- unique(vapply(vals[!empty], function(v) class(v)[1L], character(1)))
  # Whole-number cells arrive from python-calamine as Python ints and land in
  # R as integer; fold them into numeric so they don't read as a mixed type.
  target <- unique(replace(target, target == "integer", "numeric"))

  if (length(target) == 1L && target == "numeric") {
    .build_column_numeric(vals, empty)
  } else if (length(target) == 1L && target == "Date") {
    .build_column_date(vals, empty)
  } else if (length(target) == 1L && target == "logical") {
    .build_column_logical(vals, empty)
  } else {
    .build_column_text(vals, empty)
  }
}

.build_column_text <- function(vals, empty) {
  out <- vapply(
    vals,
    function(v) if (length(v) == 0L) NA_character_ else as.character(v),
    character(1)
  )
  out[empty] <- NA_character_
  out
}

.build_column_numeric <- function(vals, empty) {
  out <- vapply(
    vals,
    function(v) {
      if (length(v) == 0L) {
        return(NA_real_)
      }
      if (is.numeric(v)) {
        return(as.numeric(v))
      }
      suppressWarnings(as.numeric(as.character(v)))
    },
    numeric(1)
  )
  out[empty] <- NA_real_
  out
}

.build_column_logical <- function(vals, empty) {
  out <- vapply(
    vals,
    function(v) {
      if (length(v) == 0L) {
        return(NA)
      }
      if (is.logical(v)) {
        return(v)
      }
      suppressWarnings(as.logical(as.character(v)))
    },
    logical(1)
  )
  out[empty] <- NA
  out
}

# Only cells calamine already parsed as Date are kept; other types can't be
# unambiguously interpreted as dates, so they become NA rather than guessed at.
.build_column_date <- function(vals, empty) {
  out <- vapply(
    vals,
    function(v) if (inherits(v, "Date")) as.numeric(v) else NA_real_,
    numeric(1)
  )
  out[empty] <- NA_real_
  structure(out, class = "Date")
}
