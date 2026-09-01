## Internal reticulate setup for the python-calamine backend.

# Cached handle to the imported python_calamine module.
pc <- NULL

# Cached handle to the bundled workaround module (see .pc_san() below).
pc_san <- NULL

.onLoad <- function(libname, pkgname) {
  # Declare the Python dependency so reticulate installs it automatically
  # (via uv) the first time Python is initialised. This does not itself
  # start Python.
  reticulate::py_require("python-calamine>=0.8")

  # delay_load = TRUE means Python is not initialised until the module is
  # first actually used, so library(rxlsb) stays cheap.
  pc <<- reticulate::import("python_calamine", delay_load = TRUE)

  # rxlsb_sanitize.py is a stdlib-only workaround for a calamine parser bug
  # (see that file for details); it has no extra Python dependencies.
  pc_san <<- reticulate::import_from_path(
    "rxlsb_sanitize",
    path = system.file("python", package = "rxlsb"),
    delay_load = TRUE
  )
}

# Accessor used by the exported functions.
.pc <- function() {
  pc
}

# Accessor for the sanitize-workaround module.
.pc_san <- function() {
  pc_san
}
