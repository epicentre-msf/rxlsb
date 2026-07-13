#' List the sheets in an '.xlsb' workbook
#'
#' Returns the names of the worksheets in an Excel binary (`.xlsb`) workbook,
#' in workbook order. This is the `.xlsb` counterpart to
#' [readxl::excel_sheets()].
#'
#' @param path Path to the `.xlsb` file.
#'
#' @return A character vector of sheet names.
#'
#' @seealso [rxlsb()] to read a sheet.
#'
#' @examples
#' \dontrun{
#' list_sheets("workbook.xlsb")
#' }
#'
#' @export
list_sheets <- function(path) {
  wb <- .workbook(path)
  as.character(wb$sheet_names)
}

# Open a workbook, normalising the path first so python-calamine gets an
# absolute path and a missing file fails early with a clear R error.
#
# calamine (the Rust crate behind python-calamine) has a parser bug that can
# make it report zero sheets -- or fail outright -- for an otherwise valid
# `.xlsb` file, depending on the exact byte length of an unrelated stored
# path inside `xl/workbook.bin` (see inst/python/rxlsb_sanitize.py for the
# full explanation). When that happens we retry once against a sanitized
# in-memory copy of the archive before giving up.
.workbook <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)

  result <- tryCatch(
    list(wb = .pc()$CalamineWorkbook$from_path(path), error = NULL),
    error = function(e) list(wb = NULL, error = e)
  )

  if (!is.null(result$wb) && length(result$wb$sheet_names) > 0L) {
    return(result$wb)
  }

  wb_san <- tryCatch(
    {
      filelike <- .pc_san()$sanitize_to_filelike(path)
      .pc()$CalamineWorkbook$from_filelike(filelike)
    },
    error = function(e) NULL
  )

  if (!is.null(wb_san) && length(wb_san$sheet_names) > 0L) {
    return(wb_san)
  }

  # Sanitizing didn't help (or itself failed): surface the *original*
  # failure so the user sees calamine's real complaint about the file,
  # rather than one about our workaround.
  if (!is.null(result$error)) {
    stop(result$error)
  }
  result$wb
}
