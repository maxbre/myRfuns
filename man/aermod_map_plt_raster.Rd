% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aermod_map_plt_raster.R
\name{aermod_map_plt_raster}
\alias{aermod_map_plt_raster}
\title{aermod_map_plt_raster}
\usage{
aermod_map_plt_raster(
  input_filename,
  rows_to_skip_head,
  rows_to_skip_tail = 0,
  trans_factor = 1,
  name_of_map_layer,
  string_filename_html
)
}
\arguments{
\item{input_filename}{a string for the aermod input file name *.plt}

\item{rows_to_skip_head}{a number of rows to skip in the head of the aermod plt file}

\item{rows_to_skip_tail}{a number of rows to skip in the tail of the aermod plt file}

\item{trans_factor}{a number, transformation factor for results}

\item{name_of_map_layer}{a string}

\item{string_filename_html}{a string}
}
\value{
a map in the form html and png
}
\description{
Create mapview object from aermod plt file, save results as html and png files
}
\examples{
\dontrun{
aermod_map_plt_raster('AERMOD.PLT')
}
}
