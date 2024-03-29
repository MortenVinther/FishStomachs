% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bias_correct_energy_etc.R
\name{bias_correct_energy_etc}
\alias{bias_correct_energy_etc}
\title{Bias correct stomach content due to energy contents, armament etc.}
\usage{
bias_correct_energy_etc(
  x,
  nodc_group_file,
  param_file,
  temperature_file,
  energy_file,
  hour_multiplier = 1,
  pred_length_multiplier = 1,
  stomach_consum = c("stom", "cons")[1]
)
}
\arguments{
\item{x}{Stomach data of class \code{STOMobs}}

\item{nodc_group_file}{a}

\item{param_file}{b}

\item{temperature_file}{c}

\item{energy_file}{d}

\item{hour_multiplier}{The stomach contents prey weight (prey_w) is transformed into an eaten food ration (grammes per hour) which can be considered a as proxy for stomach contents weight.food ration
The unit, grammes per hour can be changed by the hour_multiplier, e.g \code{hour_multiplier=2190} will provide g/quarter of the year.;}

\item{pred_length_multiplier}{Multiplier for predator length (to derive the predator length used (in cm) from the predator length variable (pred_l)used in data)}

\item{stomach_consum}{bias correct stomach observations or estimate unbiased food consumption}
}
\value{
Stomach contents data of class \code{STOMobs} where the observed prey weights has been recalculated to ration eaten by hour (or any other unit chosen by \code{hour_multiplier})
}
\description{
Bias correct stomach content due to prey energy density and armament.
}
