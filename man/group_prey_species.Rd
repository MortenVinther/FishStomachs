% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_preys.R
\name{group_prey_species}
\alias{group_prey_species}
\title{Group prey species names.}
\usage{
group_prey_species(
  stom,
  keep_prey_names,
  sum_other_food = TRUE,
  NODC_split,
  show_allocations = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{stom}{Stomach contents data of class STOMobs.}

\item{keep_prey_names}{Names of preys to be kept unchanged. Remaining preys are renamed to "other" and their prey sizes are changed to missing.}

\item{sum_other_food}{Sum  stomach contents of "other" preys.}

\item{NODC_split}{Input file with the fields XXXXX}

\item{show_allocations}{If true the allocation of prey names into groups are shown, when the \code{NODC_split} is used.
in the control object are renamed into "other".}

\item{verbose}{Show extra information}
}
\value{
Stomach contents data of class STOMobs. Prey sizes and number of preys as set to missing for preys in the "other" food group.
}
\description{
Prey species are renamed and re-grouped into larger groups of prey, with summed prey weights. The simplest approach maintain the species names provided
by \code{keep_prey_names} and rename all other preys into "other". A more complex method uses a key based on the NODC to split (see \code{NODC_split}).
}
