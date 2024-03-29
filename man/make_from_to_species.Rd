% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redistribution.R
\name{make_from_to_species}
\alias{make_from_to_species}
\title{Read csv-file with allocation keys for partly identified prey species.}
\usage{
make_from_to_species(inp_file = "from_to_species.csv")
}
\arguments{
\item{inp_file}{Input file name. The file must include the variable names order, from_species and to_species.}
}
\value{
Data frame with names of partly identified preys and their allocation into identified preys
}
\description{
Read csv-file with allocation keys for partly identified prey species.
}
\examples{
\dontrun{from_to<-make_from_to_species();}

}
