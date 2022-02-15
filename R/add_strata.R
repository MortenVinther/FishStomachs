#' Add temporal and spatial strata as specified in a control object \link{STOMcontrol-class} for collation of stomach data.
#' @title  Add temporal and spatial strata to STOMobs
#' @param stom Stomach data set of class STOMobs.
#' @return STOMobs object with added strata.
#' @export
#' @examples \dontrun{a<-add_strata(a)}
add_strata <- function(stom) {
  control<-attr(stom,'control')
  stratum_time<-with(stom[['PRED']],eval(control@strata_time_exp))
  stratum_area<-with(stom[['PRED']],eval(control@strata_area_exp))
  stratum_sub_area<-with(stom[['PRED']],eval(control@strata_sub_area_exp))
  stom[['PRED']] <-stom[['PRED']] %>%
      dplyr::mutate(stratum_time=forcats::fct_explicit_na(stratum_time),stratum_area=forcats::fct_explicit_na(stratum_area),
                    stratum_sub_area=forcats::fct_explicit_na(stratum_sub_area))
  attr(stom,all_stom_attributes()['strata'])<-TRUE
  return(stom)
}
