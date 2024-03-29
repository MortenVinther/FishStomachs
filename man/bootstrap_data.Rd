% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrapping.R
\name{bootstrap_data}
\alias{bootstrap_data}
\title{Make bootstrap replicates.}
\usage{
bootstrap_data(
  s,
  nfac = 1,
  replace = TRUE,
  seed = 0,
  rep_id = 1,
  firstUnchanged = TRUE
)
}
\arguments{
\item{s}{Stomach data set of class STOMobs.}

\item{nfac}{Number of bootstrap samples relative to original number of samples.}

\item{replace}{Logical for bootstrapping with replacement.}

\item{seed}{Random seed value.}

\item{rep_id}{Replicate identifier}

\item{firstUnchanged}{Do not bootstrap for rep_id==1}
}
\value{
Bootstrapped stomach data (replicate) of class STOMobs.
}
\description{
Make bootstrap replicates of STOMobs object from stratification specified by control object, see (\link{STOMcontrol-class}  ).
}
\examples{
\dontrun{b<-bootstrap_data(s)}
}
