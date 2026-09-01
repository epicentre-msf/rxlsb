# Builds a minimal, synthetic one-sheet `.xlsb` workbook in memory, so the
# calamine bug worked around in R/list_sheets.R (see .workbook() there and
# inst/python/rxlsb_sanitize.py for the full explanation) can be regression-
# tested without a real (and, for this package's actual use case, sensitive)
# sample workbook on disk.
#
# The bug is triggered by the byte length of the workbook's stored "last
# saved from" path (the BrtAbsPath15 record in xl/workbook.bin): when that
# path is exactly 70 characters, calamine mis-parses the record stream and
# reports zero sheets. build_minimal_xlsb() lets a test construct a workbook
# with a chosen path length, to reproduce (or avoid) the bug on demand.
#
# Only the handful of BIFF12 records calamine's reader actually consumes are
# written; this is not a general-purpose xlsb writer.

# One BIFF12 record: a 1-2 byte record-type varint, a 1-4 byte length varint,
# then the payload. Mirrors the reader in inst/python/rxlsb_sanitize.py.
.biff_record <- function(type, payload) {
  type_bytes <- if (type < 0x80) {
    as.raw(type)
  } else {
    as.raw(c((type %% 128) + 128, type %/% 128))
  }
  len <- length(payload)
  len_bytes <- raw(0)
  repeat {
    b <- bitwAnd(len, 0x7F)
    len <- bitwShiftR(len, 7)
    if (len > 0) {
      len_bytes <- c(len_bytes, as.raw(bitwOr(b, 0x80)))
    } else {
      len_bytes <- c(len_bytes, as.raw(b))
      break
    }
  }
  c(type_bytes, len_bytes, payload)
}

.u32le <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")

.utf16le <- function(s) iconv(s, to = "UTF-16LE", toRaw = TRUE)[[1]]

# MS-XLSB "XLWideString": a 4-byte character count followed by that many
# UTF-16LE characters (no null terminator).
.wide_str <- function(s) c(.u32le(nchar(s)), .utf16le(s))

# xl/workbook.bin: BrtWbProp, BrtAbsPath15 (the record whose length triggers
# the bug), one BrtBundleSh sheet entry, BrtEndBundleShs, BrtEndBook.
.build_workbook_bin <- function(abspath, sheet_name = "data") {
  wbprop <- .biff_record(0x0099, raw(4)) # not 1904-based

  abspath_rec <- .biff_record(0x0817, .wide_str(abspath)) # BrtAbsPath15

  bundlesh_payload <- c(
    .u32le(0L), # hsState: visible
    .u32le(1L), # iTabID
    .u32le(4L), # cchRelId (character count of "rId1")
    .utf16le("rId1"),
    .wide_str(sheet_name)
  )
  bundlesh <- .biff_record(0x009C, bundlesh_payload) # BrtBundleSh

  c(
    wbprop,
    abspath_rec,
    bundlesh,
    .biff_record(0x0090, raw(0)), # BrtEndBundleShs
    .biff_record(0x0084, raw(0)) # BrtEndBook
  )
}

# xl/worksheets/sheet1.bin: BrtWsDim, BrtBeginSheetData, one BrtRowHdr +
# BrtCellRk pair per cell, BrtEndSheetData. Cell values are small integers
# encoded via the RK "signed integer" variant (fInt bit set).
# `data_rows` gives the 0-based sheet row of each of the `nrow` data rows,
# letting a test place data below empty rows or leave gaps; cell values
# depend only on a row's position within `data_rows`, not its sheet row.
.build_sheet1_bin <- function(nrow = 3L, ncol = 2L, data_rows = seq_len(nrow) - 1L) {
  stopifnot(length(data_rows) == nrow)
  wsdim <- .biff_record(0x0094, c(
    .u32le(min(data_rows)), .u32le(max(data_rows)),
    .u32le(0L), .u32le(ncol - 1L)
  ))
  begin_data <- .biff_record(0x0091, raw(0))

  rows <- raw(0)
  for (i in seq_len(nrow) - 1L) {
    rows <- c(rows, .biff_record(0x0000, .u32le(data_rows[[i + 1L]]))) # BrtRowHdr
    for (cl in seq_len(ncol) - 1L) {
      value <- (i * ncol + cl + 1L) * 10L
      rk <- bitwOr(bitwShiftL(as.integer(value), 2L), 0x2L) # fInt = 1
      cell_payload <- c(.u32le(cl), .u32le(0L), .u32le(rk))
      rows <- c(rows, .biff_record(0x0002, cell_payload)) # BrtCellRk
    }
  }
  c(wsdim, begin_data, rows, .biff_record(0x0092, raw(0))) # BrtEndSheetData
}

.xlsb_fixture_rels_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" ',
  'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" ',
  'Target="worksheets/sheet1.bin"/></Relationships>'
)

# Build a minimal one-sheet `.xlsb` file, named "data", with `nrow` x `ncol`
# integer cells (the first data row is 10, 20, ...; each subsequent row +10
# per column step), and a BrtAbsPath15 record encoding `abspath`. `data_rows`
# optionally places the data rows at specific 0-based sheet rows (see
# .build_sheet1_bin). Returns the path to a temp file. Uses Python's stdlib
# `zipfile` (via reticulate) purely as a zip writer -- rxlsb already requires
# Python at runtime.
build_minimal_xlsb <- function(abspath, nrow = 3L, ncol = 2L,
                               data_rows = seq_len(nrow) - 1L) {
  zipfile <- reticulate::import("zipfile")

  out_path <- tempfile(fileext = ".xlsb")
  zf <- zipfile$ZipFile(out_path, "w", zipfile$ZIP_DEFLATED)
  zf$writestr("xl/workbook.bin", .build_workbook_bin(abspath))
  zf$writestr("xl/_rels/workbook.bin.rels", .xlsb_fixture_rels_xml)
  zf$writestr("xl/worksheets/sheet1.bin", .build_sheet1_bin(nrow, ncol, data_rows))
  zf$close()
  out_path
}
