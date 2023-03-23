#' Stomach contents are aggregated and predator information are updated for samples with the same \code{sample_id} and one or more fish \code{fish_id}.
#'
#' The stomach contents weight by prey and prey_size class are summed for for all samples within a sample_id and predator size class.
#' Number of stomachs are similarly added to include all stomachs, however only the total number of stomachs \code{n_tot} is maintained.
#' In the aggregated data, the variable \code{fish_id} becomes the value of predator size.
#'
#' Size classes of both predators and preys must have been assigned ( \link{put_size_class_on_predator} and \link{put_size_class_on_prey} ) before aggregation can be made.
#'
#' @title Aggregate stomach contents within sample
#' @param stom  stomach contents data of class STOMobs.
#' @return stomach contents data of class STOMobs with aggregated stomach contents and number of stomachs, and updated predator information.
#' @export
#' @examples \dontrun{aggregate_within_sample(ns)}
aggregate_within_sample <- function(stom) {

  # to avoid "no visible binding for global variable" note
  sample_id<-fish_id<-pred_size<-pred_l_mean<-pred_name<-pred_size_class<-pred_cpue<-nl<-pred_l<-pred_ll<-pred_lu<-NULL
  record_type<-prey_l_mean<-prey_name<-digest<-prey_size_class<-prey_size_class<-prey_size<-prey_w<-prey_n<-prey_l_mean_num<-n_tot<-NULL

  control<-attr(stom,'control')
  mis_l<-as.integer(control@mis_l)

  if (!('prey_size' %in% colnames(stom[['PREY']])  & 'pred_size' %in% colnames(stom[['PRED']]))) stop('Error: Predator and prey sizes classes must have been added\n')

  # add pred_size to PREY
  prey<-dplyr::left_join(stom[['PREY']],dplyr::select(stom[['PRED']],sample_id,fish_id,pred_size), by = c("sample_id", "fish_id"))

  add_cols <- function(df, cols) {
    addC <- cols[!cols %in% names(df)]
    if(length(addC) !=0 ) {
      df[addC] <- 0L
      cat("the variables: ", addC, " has been added (set to 0) to make bias correction for regurgitated stomachs\n")
    }
    return(df)
  }


  # add 'n_food', 'n_regur', 'n_skel' and 'n_empty' if they don't already exist
  #stom[['PRED']]<- add_cols(stom[['PRED']], c('n_food', 'n_regur', 'n_skel', 'n_empty'))


  pred<-stom[['PRED']]   ## %>% dplyr::mutate(n_food=NULL)

  # aggregate number of stomachs within sample_id and pred_size
  nstom<-pred %>% dplyr::mutate(nl=n_tot*pred_l_mean)  %>% dplyr::group_by(pred_name,sample_id,pred_size_class,pred_size) %>%
    dplyr::summarise(n_tot=sum(n_tot),nl=sum(nl)) %>%
    dplyr::mutate(pred_l_mean=as.integer(nl/n_tot),nl=NULL) %>% dplyr::ungroup()


  if ("pred_cpue" %in% colnames(pred)) {
    cpue<-pred %>% dplyr::group_by(pred_name,sample_id,pred_size_class,pred_size) %>%
      dplyr::summarise(pred_cpue=mean(pred_cpue,na.rm=TRUE)) %>% dplyr::ungroup()
    pred<- dplyr::select(pred, -pred_cpue)
    nstom<-dplyr::left_join(nstom,cpue, by = c("pred_name", "sample_id", "pred_size_class", "pred_size"))
  }
  # add remaining variables to PRED
  pred<-pred %>% dplyr::select(-fish_id,-pred_l,-pred_ll,-pred_lu,-n_tot,-pred_l_mean,-record_type) %>% dplyr::distinct()
  pred<-dplyr::left_join(pred,nstom,  by = c("sample_id", "pred_name", "pred_size_class", "pred_size"))


  # aggregate stomach contents within sample_id and pred_size
  prey<-prey %>% dplyr::mutate(prey_l_mean_num=dplyr::if_else(prey_l_mean==mis_l,as.integer(NA),prey_l_mean))   %>%
    dplyr::group_by(sample_id, pred_size, prey_name,digest, prey_size_class, prey_size) %>%
    dplyr::summarise(prey_w=sum(prey_w,na.rm=TRUE),prey_l_mean=sum(prey_n*prey_l_mean_num),prey_n=sum(prey_n)) %>%
    dplyr::mutate(prey_l_mean=as.integer(prey_l_mean/prey_n)) %>% dplyr::rename(fish_id=pred_size)

  stom[['PRED']]<-pred %>% dplyr::ungroup() %>% dplyr::mutate(fish_id=forcats::fct_explicit_na(pred_size))
  stom[['PREY']]<-prey %>% dplyr::ungroup() %>% dplyr::mutate(fish_id=factor(fish_id,levels=levels(stom[['PRED']]$fish_id)))

  attr(stom,all_stom_attributes()['aggregate_id'])<-TRUE
  return(stom)
}


