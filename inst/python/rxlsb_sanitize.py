"""Work around a parser bug in the `calamine` Rust crate (used by
python-calamine) that can make it report zero sheets for an otherwise valid
`.xlsb` workbook.

Background
----------
`xl/workbook.bin` in an `.xlsb` file is a stream of BIFF12 records, each
shaped as: a 1-2 byte record-type varint, a 1-4 byte length varint, then
`length` bytes of payload. calamine's workbook reader recognises a handful of
record types it cares about (BrtWbProp, BrtBundleSh, BrtEndBundleShs, ...)
and for every other record type just reads the type and does *nothing* --
crucially, it never reads (and skips) that record's length + payload. So the
next bytes it reads as "the next record type" are actually the tail of the
record it just ignored.

One commonly-present record is `BrtAbsPath15` (type 0x0817): the absolute
path from which the workbook was last saved. calamine does not handle it, so
it is silently mis-skipped as above. If that stored path happens to be
exactly 70 UTF-16 characters long, the record's payload is
`4 + 2*70 = 144` bytes, which is encoded as the two-byte length varint
`0x90 0x01`. Landing on that `0x90` as if it were a record *type* decodes to
`0x0090`, which is `BrtEndBundleShs` -- the sentinel that tells calamine
"stop, there are no more sheets". The result: calamine reports zero sheets,
even though the workbook is completely valid and Excel (and other readers)
open it fine.

This is a genuine upstream bug (still present as of calamine v0.36.0,
2024- ...; see `read_workbook` in `src/xlsb/mod.rs`), not something wrong
with the file. `BrtAbsPath15` carries no information needed to read cell
data, so the workaround is simply to strip it (and any other record type
calamine doesn't need) out of `xl/workbook.bin` before handing the archive
to calamine.

Why not switch to pyxlsb instead?
----------------------------------
`pyxlsb` is a separate, pure-Python `.xlsb` parser that does not have this
bug, so it looks like an easy way to drop this workaround entirely. It was
evaluated and rejected:

  - It does not recognise dates. Cells come back as raw Excel serial floats;
    callers must inspect each cell's number format and call
    `pyxlsb.convert_date()` themselves. rxlsb's type layer
    (`.build_column_guess()` / `.build_column_date()` in R/rxlsb.R) relies on
    calamine handing back already-typed cells (native dates, numbers,
    booleans, text) -- switching would silently break automatic date
    detection, which matters a lot for date-heavy line-list data.
  - It is effectively unmaintained (last release 1.0.10, October 2022, no
    activity since), while calamine is actively developed and is pandas'
    preferred `.xlsb` engine.
  - It is meaningfully slower than calamine.
  - Confirmed directly against calamine's `master` branch: this bug is still
    unfixed as of the latest release (v0.36.0), so pinning a newer
    python-calamine isn't a way to drop this workaround either -- the retry
    logic in `.workbook()` (R/list_sheets.R) is still required.

The right long-term fix is upstream: teach calamine's `read_workbook` to
consume the length + payload of record types it doesn't recognise (mirroring
what `next_skip_blocks` already does), which would make this whole module
unnecessary. That has not been attempted yet.
"""

import io
import zipfile

# Record types calamine's workbook reader does not need to find sheets or
# cell data, but which its own skip logic can mis-parse. BrtAbsPath15 is the
# only one observed to trigger the bug in practice, but any record between
# BrtWbProp and BrtEndBundleShs that calamine doesn't handle is a latent risk
# for the same failure mode, so the walker below is general-purpose.
_DEFAULT_DROP_TYPES = (0x0817,)  # BrtAbsPath15


def _iter_records(data):
    """Yield (record_type, payload_start, payload_end) for each BIFF12
    record in `data`, per the MS-XLSB record framing (1-2 byte type varint,
    1-4 byte length varint)."""
    i = 0
    n = len(data)
    while i < n:
        start = i
        b = data[i]
        i += 1
        if b & 0x80:
            b2 = data[i]
            i += 1
            rtype = (b & 0x7F) | ((b2 & 0x7F) << 7)
        else:
            rtype = b

        size = 0
        shift = 0
        for _ in range(4):
            sb = data[i]
            i += 1
            size |= (sb & 0x7F) << shift
            shift += 7
            if not (sb & 0x80):
                break

        payload_start = i
        payload_end = i + size
        yield rtype, start, payload_start, payload_end
        i = payload_end


def strip_records(data, drop_types=_DEFAULT_DROP_TYPES):
    """Return `data` (the bytes of an `xl/workbook.bin` stream) with every
    record whose type is in `drop_types` removed entirely (header and
    payload). Dropping the whole record -- rather than, say, patching its
    length -- is robust: a byte pair that misreads as BrtEndBundleShs can
    arise from payload content too, so shortening the payload wouldn't
    reliably fix it, whereas removing the record removes the hazard."""
    out = bytearray()
    for rtype, start, _payload_start, payload_end in _iter_records(data):
        if rtype in drop_types:
            continue
        out += data[start:payload_end]
    return bytes(out)


def sanitize_to_filelike(src_path, drop_types=_DEFAULT_DROP_TYPES):
    """Open the `.xlsb` zip archive at `src_path`, replace `xl/workbook.bin`
    with a copy that has had `drop_types` records removed, and return the
    rebuilt archive as an in-memory, seeked-to-0 `io.BytesIO` ready to hand
    to `CalamineWorkbook.from_filelike()`. Every other archive member is
    copied through unchanged."""
    out = io.BytesIO()
    with zipfile.ZipFile(src_path) as zin:
        workbook_bin = zin.read("xl/workbook.bin")
        sanitized = strip_records(workbook_bin, drop_types)
        with zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as zout:
            for item in zin.infolist():
                data = sanitized if item.filename == "xl/workbook.bin" else zin.read(item.filename)
                zout.writestr(item, data)
    out.seek(0)
    return out
