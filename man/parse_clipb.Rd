% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distrb.R
\name{parse_clipb}
\alias{parse_clipb}
\title{Parse the contents in clipboard to form a tibble}
\usage{
parse_clipb(var_row = 1L, col_names = var_row > 0, col_types = NULL,
  locale = readr::default_locale(), na = "NA", n_max = Inf,
  guess_max = min(n_max, 1000), progress = readr::show_progress(),
  comment = "", skip_empty_rows = TRUE)
}
\arguments{
\item{var_row}{integer, the row index of the header row. Default 1. If no header
is needed, put it 0. If NULL, it will guess the colnames.}

\item{col_names}{either TRUE, FALSE or a character vector of column names.}

\item{col_types}{either of NULL, a cols() specification, or a string.
See vignette("readr") for more details.}

\item{locale}{the locale controls defaults that vary from place to place.}

\item{n_max}{maximum number of records to read.}

\item{guess_max}{maximum number of records to use for guessing column types.}

\item{progress}{display a progress bar? By default it will only display in an
interactive session and not while knitting a document.}

\item{skip_empty_rows}{should blank rows be ignored altogether? i.e. If this
option is TRUE then blank rows will not be represented at all. If it is FALSE
then they will be represented by NA values in all the columns.}
}
\value{
A tibble (coerced using \code{\link{read_table2}}). If fails, return NULL
}
\description{
Tables copied from spreadsheet or similar software into clipboard are usually
a character vector containing '\\t'. This function parses the clipboard and tries
to coerce the string vector to a \pkg{\link{tibble}}. It is a wrapper of
\pkg{readr}:\code{\link{read_table2}}. The arguments directly come from
\code{read_table2}.
}
\examples{
\dontrun{
parse_clipb()
}

}
\seealso{
\code{\link[readr]{read_table2}} and \code{\link[readr]{clipboard}}
}
\author{
Yiying Wang, \email{wangy@aetna.com}
}
