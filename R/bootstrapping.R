#' Make bootstrap data from stratification specified by control object, see (   ).
#'
#' @title Check if data are ready for aggregation.
#' @param s Stomach data set of class STOMobs.
#' @param nfac Number of bootstrap samples relative to original number of samples.
#' @param replace Logical for bootstrapping with replacement.
#' @param seed Random seed value.
#' @return s Bootstrapped stomach data set of class STOMobs.
#' @export
#' @examples \dontrun{check_for_missing_info_before_strata_aggregation(stom)}
bootstrap_data<-function(s,nfac=1,replace=TRUE,seed=0) {

 control<-get_control(s)
 set.seed(seed)
 s[['PRED']]<- s[['PRED']] %>%  
   dplyr::mutate(boots_id=eval(control@bootstrapping$boots_id),
                 boots_strata=eval(control@bootstrapping$boots_strata))
 ss<-as.data.frame(s)
 
 #make id from 1 to ... and draw samples for each strata
  a<-by(ss,list(ss$boots_strata),function(x) {
   x<-dplyr::mutate(x,boots_id=as.integer(factor(boots_id))) 
   n<-as.integer(max(x$boots_id))
   samples<-sample.int(n,size=as.integer(n*nfac),replace=replace)
   samp<-data.frame(boots_id=samples) %>% dplyr::group_by(boots_id) %>% 
     dplyr::mutate(i=1:n()) %>% dplyr::ungroup() 
   x<-dplyr::inner_join(x,samp,by = "boots_id") %>% 
      dplyr::mutate(sample_id=factor(paste(sample_id,i)),fish_id=factor(paste(fish_id,i)))
 })

  x<-do.call(rbind,a); 
  as_STOMobs(x) 
}

#' Show stratification specified by control object, see (   ).
#'
#' @title Show bootrap stratification.
#' @param s Stomach data set of class STOMobs.
#' @param show Show either number of stomachs per strata or number of stomachs per samples available for bootstrapping.
#' @return table Information og data available for bootstrapping
#' @export
#' @examples \dontrun{check_for_missing_info_before_strata_aggregation(stom)}
bootstrap_show<-function(s,show=c("strata",'sample')[1]) {
  control<-get_control(s)
  a<- s[['PRED']] %>%  
    dplyr::mutate(boots_id=eval(control@bootstrapping$boots_id),
                  boots_strata=eval(control@bootstrapping$boots_strata))
  if ("pred_l" %in% names(a)) a$size<-a$pred_l else a$size<-a$pred_size
 if (show=="strata") return(xtabs(n_tot ~boots_strata+pred_size+pred_name,data=a ))
 if (show=="sample") return(xtabs(n_tot ~boots_id+pred_size+pred_name,data=a ))
} 

