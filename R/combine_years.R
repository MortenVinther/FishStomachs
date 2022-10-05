#' Combine years of stomach data, e.g. assign stomach data from 2014 and 2015 to the same year, e.g. 2014
#' @title Combine years of stomach data.
#'
#' @param stom Stomach data set of class STOMobs, from e.g. \link{read_exchange_data}.
#' @param year_file File Name for csv file including the variable year_old, year_new and predator.
#' @param verbose Logical for showing details
#' @return Stomach data of class STOMobs with where the years are changed according to the specifications in file year_file..
#' @export
#' @examples \dontrun{combine_year(s,year_file="combine_year_10.csv")}
combine_years <- function(stom, year_file,verbose=FALSE) {
  if (verbose) cat("year_file: ", year_file,'\n')
  cat(file.exists(year_file),'\n')
  if (rlang::is_missing(year_file)) {  if (verbose) cat('\nNo year_file was specified. Nothing is changed\n'); return(stom) }
  if (!file.exists(year_file)) {
    if (verbose) cat('\nFile ',year_file,' was not found. Nothing is changed\n' )
    return(stom)
  } else  b <- utils::read.csv(year_file, strip.white = TRUE, stringsAsFactors = FALSE)

  stom[['PRED']]<-dplyr::left_join(stom[['PRED']],b, by=c('year'='year_old','pred_name'='predator'))
  stom[['PRED']]<-stom[['PRED']] %>% mutate(year=if_else(is.na(year_new),year,as.integer(year_new)),year_new=NULL,pred_name=as.factor(pred_name))
  return(stom)
}

