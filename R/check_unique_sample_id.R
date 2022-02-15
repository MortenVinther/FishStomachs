#' Check that within one combination of sample_id and fish_id there is only one set of predator information.
#' @title Check uniqueness of sample_id and fish_id
#' @param stom Stomach data using internal variable names.
#' @return data frame with potential issues.
#' @examples \dontrun{problems <- check_unique_sample_id(stom=a)}
check_unique_sample_id<-function(stom){
  fish_id<-fish_id<-mandatory<-mandatory<-n_sample_id<-n_sample_id<-PRED<-PRED<-sample_id<-sample_id<-NULL
  control<-attr(stom,'control')
  stomach_format<- eval(control@stomach_format)
  b <- read.csv(file = stomach_format, stringsAsFactors = FALSE)
  PRED_fields<-subset(b,mandatory & PRED)$field

  a1<-stom[['PRED']] %>% dplyr::group_by(sample_id,fish_id) %>% dplyr::summarise(n_sample_id=dplyr::n())
  err1<-dplyr::filter(a1,n_sample_id>1)
  ok<-dplyr::filter(a1,n_sample_id==1)
  if (dim(a1)[[1]] !=  dim(stom[['PRED']])[[1]]) {
    print('Error, combinations of sample_id and fish_id are not unique')
    bb<-dplyr::anti_join(stom[['PRED']],ok)
  } else bb<-stom[['PRED']][0,]
  return(bb)
}

#' Check uniqueness of diet_id (key)
#' @title Check uniqueness of diet_id (key)
#' @param diet Object of class STOMdiet.
#' @return data frame with potential issues.
#' @examples \dontrun{problems <- check_unique_diet_id(diet=a)}
check_unique_diet_id<-function(diet){

  key<-n_diet_id<-NULL

  a1<-diet[['PRED']] %>% dplyr::group_by(key) %>% dplyr::summarise(n_diet_id=dplyr::n())
  err1<-dplyr::filter(a1,n_diet_id>1)
  ok<-dplyr::filter(a1,n_diet_id==1)
  if (dim(a1)[[1]] !=  dim(diet[['PRED']])[[1]]) {
    print('Error, keys not unique')
    bb<-dplyr::anti_join(diet[['PRED']],ok,by="key")
  } else bb<-diet[['PRED']][0,]
  return(bb)
}
