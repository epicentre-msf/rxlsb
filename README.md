
# rxlsb: Read .xlsb files into R

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/epicentre-msf/rxlsb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epicentre-msf/rxlsb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An alternative to the archived [readxlsb](https://github.com/velofrog/readxlsb) package 
with an interface more similar to [readxl](https://readxl.tidyverse.org/). Uses the Python
package [python-calamine](https://github.com/dimastbk/python-calamine) via
[reticulate](https://rstudio.github.io/reticulate/).

## Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/rxlsb")
```

Requires Python (>= 3.10). The Python dependency (`python-calamine`) is
installed automatically on first use.

## Usage

```r
library(rxlsb)

# list sheet names
list_sheets("workbook.xlsb")

# read a sheet into a tibble (defaults to the first sheet)
rxlsb("workbook.xlsb")
rxlsb("workbook.xlsb", sheet = "data")

# readxl-style options
rxlsb("workbook.xlsb", col_names = FALSE)
rxlsb("workbook.xlsb", col_types = c("text", "numeric", "date"), na = c("", "NA"))
rxlsb("workbook.xlsb", skip = 2)
```