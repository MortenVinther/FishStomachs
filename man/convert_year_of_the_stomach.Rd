% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ICES_year_of_the_stomach.R
\name{convert_year_of_the_stomach}
\alias{convert_year_of_the_stomach}
\title{Read ICES year-of-the-stomach data}
\usage{
convert_year_of_the_stomach(
  dir,
  in_file,
  NODC_latin,
  latin_prey_names = TRUE,
  write_file = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{dir}{Directory with in_file}

\item{in_file}{Name or names of files with data on the ICES Year-of-the-stomach exchange format.}

\item{NODC_latin}{CSV file with NODC and latin names. Default file system.file("NODC_latin.csv", package = "FishStomachs").}

\item{latin_prey_names}{Add latin names on preys from NODC}

\item{write_file}{Write file with data on exchange format, file name becomes "adjusted_"+in_file}

\item{verbose}{Show information on processing data.}
}
\value{
stomach data on exchange format
}
\description{
Read ICES year-of-the-stomach data
}
