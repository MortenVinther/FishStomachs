#' Change stomach data
#' @title Make changes on  STOMobs from user options
#' @param stom Stomach data set of class STOMobs.
#' @param delete_vars vector of variable names to be deleted.
#' @param control_criteria dplyr::select sub-set of data from criteria given in control.
#' That is criteria given by the slots predators, years, stratum_areas and stratum. See \linkS4class{STOMcontrol}.
#' @param pred_weight_multiplier Multiplier for predator body weight, eg.g from kg to grammes.
#' @param pred_length_multiplier Multiplier for predator body length, from cm to mm.
#' @param prey_weight_multiplier Multiplier for prey body weight.
#' @param prey_length_multiplier Multiplier for prey body length.
#' @param insert_quarter Insert quarter of the year field from date.
#' @param correct_nstom  Insert values for number of stomachs (total, with food, regurgitated, empty) where missing.
#' @param correct_predl  Insert "missing" \code{control@mis_l} values for predator length range (pred_ll and pred_lu) from pred_l where missing. Insert  missing pred_l from mean of pred_ll and pred_lu.
#' @param correct_preyl  Insert "missing" \code{control@mis_l} values for prey length (prey_l) and range of prey  (prey_ll and prey_lu) where missing.
#' @param add_weight_method  Add code,if missing, for method of weighing and recording the weights of preys item (r=by record, p=by prey species and digestion stage).
#' @param add_record_type  Add code, if missing, for sampling method for stomachs (SS=Single stomachs, PS=Pooled stomachs).
#' @param reorganize_keys Change internal keys (\code{sample_id}, \code{fish_id}). Do not change into TRUE!.
#' @return STOMobs object.
#' @examples \dontrun{read_exchange_data(stom_dir = 'NorthSea', exchange_file = 'cod_stomachs_2017.dat')}
#' @importFrom forcats fct_explicit_na
#' @export
change_data <- function(stom,delete_vars,control_criteria=FALSE,
                        pred_weight_multiplier=1,pred_length_multiplier=1,prey_weight_multiplier=1,prey_length_multiplier=1,
                        insert_quarter=TRUE, correct_nstom=TRUE,correct_predl=TRUE,correct_preyl=TRUE,
                        add_weight_method=FALSE,add_record_type=TRUE,reorganize_keys=FALSE) {

  #  stom<-s; delete_vars=c('ship','month','day','time','haul','station');pred_weight_multiplier=1;pred_length_multiplier=1;prey_weight_multiplier=1;prey_length_multiplier=1;insert_quarter=TRUE;correct_nstom=TRUE;correct_predl=TRUE;correct_preyl=TRUE;control_criteria=FALSE;reorganize_fields=TRUE;reorganize_keys=FALSE;add_weight_method=TRUE;add_record_type=TRUE;
  fish_id<-fish_id2<-month<-n<-n_tot<-PRED<-pred_name<-PREY<-prey_name<-prey_pool_id<-prey_w<-quarter<-record_type<-sample_id<-sample_id2<-year<-NULL

  control<-attr(stom,'control')
  mis_l<-as.integer(control@mis_l)

  # changes

  if (insert_quarter) {
    if (!('month' %in% colnames(stom[['PRED']]))) cat('Dataset does not include the variable "month". Quarter cannot be derrived!\n') else {
     stom[['PRED']]<- stom[['PRED']] %>% dplyr::mutate(quarter=dplyr::if_else(is.na(quarter),as.integer((month+2) %/% 3),quarter))
    no_q<-dplyr::filter(stom[['PRED']],!(quarter %in% (1:4)))
    if (dim(no_q)[[1]]>0) {
      print(paste0(dim(no_q)[[1]],' records were deleted as the quarter value (or quarter derived from month) was not in ',paste0(control@quarters,collapse=', ')))
      del_id<-no_q$sample_id
      stom<-subset(stom,!(sample_id %in% del_id))
    }}
  }

  if (pred_length_multiplier!=1) stom[['PRED']]$pred_l<-stom[['PRED']]$pred_l*pred_length_multiplier
  if (pred_weight_multiplier!=1) stom[['PRED']]$pred_w<-stom[['PRED']]$pred_w*pred_weight_multiplier

  if (prey_length_multiplier!=1) stom[['PREY']]$prey_l<-stom[['PREY']]$prey_l*prey_length_multiplier
  if (prey_weight_multiplier!=1) stom[['PREY']]$prey_w<-stom[['PREY']]$prey_w*prey_weight_multiplier


  if (correct_nstom) {
    st<-stom[['PRED']]
    if (!any(grepl("n_tot",colnames(st)))) st$n_tot<-as.integer(NA)
    st[is.na(st$n_food),'n_food']<-0
    st[is.na(st$n_regur),'n_regur']<-0
    st[is.na(st$n_skel),'n_skel']<-0
    st[is.na(st$n_empty),'n_empty']<-0
    mis<-is.na(st$n_tot)
    st[mis,'n_tot'] <-  st[mis,'n_food'] +st[mis,'n_regur'] +  st[mis,'n_skel'] +  st[mis,'n_empty']
    st<-dplyr::filter(st,n_tot>=1)
    stom[['PRED']]<-st
  }

  if (add_record_type  & !("record_type" %in% colnames(stom[['PREY']]))) {
    if (!any(grepl("record_type",colnames(stom[['PRED']])))) stom[['PRED']]$record_type<-as.character(NA)
    stom[['PRED']]$record_type<-as.character( stom[['PRED']]$record_type)
    stom[['PRED']] <- stom[['PRED']] %>% dplyr::mutate(record_type=dplyr::if_else(is.na(record_type),dplyr::if_else(n_tot>1,'PS','SS'),record_type)) %>%
          dplyr::mutate( record_type=forcats::fct_explicit_na(record_type))
  }

  if (add_weight_method & !("prey_w_meth" %in% colnames(stom[['PREY']]))) {
    stom[['PREY']]<- stom[['PREY']] %>% dplyr::mutate(prey_pool_id=factor(paste(sample_id,fish_id,prey_name,prey_w)))
    b<- stom[['PREY']] %>% dplyr::select(prey_pool_id) %>%  dplyr::group_by(prey_pool_id) %>% dplyr::summarise(n=dplyr::n())
    stom[['PREY']]<-dplyr::left_join(stom[['PREY']],b, by = "prey_pool_id") %>% dplyr::mutate(prey_w_meth=factor(dplyr::if_else(n>1,'p','r')),n=NULL,prey_pool_id=NULL)
  }

  if ("prey_w_meth" %in% colnames(stom[['PREY']])) if (all(stom[['PREY']]$prey_w_meth=='r'))   attr(stom,all_stom_attributes()["prey_w_id"])<-TRUE

  if (correct_predl) {
    st<-stom[['PRED']]
    st[is.na(st$pred_ll),'pred_ll']<- st[is.na(st$pred_ll),'pred_l']
    st[is.na(st$pred_lu),'pred_lu']<- st[is.na(st$pred_lu),'pred_l']
    st[is.na(st$pred_l),'pred_l']<- round(((st[is.na(st$pred_l),'pred_ll']+st[is.na(st$pred_l),'pred_lu'])/2))

    stom[['PRED']]<-st
  }
  ## ?  stom[['PRED']][is.na(stom[['PRED']]$pred_lu),'pred_lu']<-  stom[['PRED']][is.na(stom[['PRED']]$pred_lu),'pred_lu']+1


  if (correct_preyl) {
    st<-stom[['PREY']]
    st[is.na(st$prey_l),'prey_l']<-mis_l
    st[is.na(st$prey_ll),'prey_ll']<- as.integer(unlist(st[is.na(st$prey_ll),'prey_l']))
    st[is.na(st$prey_lu),'prey_lu']<- as.integer(unlist(st[is.na(st$prey_lu),'prey_l']))
    stom[['PREY']]<-st
  }

  # delete
  fields <- sapply(stom,colnames)
  if (!missing(delete_vars)) for (pp in c('PRED','PREY')) for (v in delete_vars) if (v %in% fields[[pp]]) stom[[pp]][,v]<-NULL

  # reorganise sample_id and prey_id
  if (reorganize_keys) {
    stom[['PRED']] <- stom[['PRED']]  %>% dplyr::group_by(pred_name) %>% dplyr::mutate(sample_id2=1:dplyr::n()) %>%  dplyr::ungroup() %>%
      dplyr::mutate(sample_id2=factor(paste(pred_name,sample_id2,sep='_'))) %>%
      dplyr::group_by(pred_name) %>% dplyr::mutate(fish_id2=1:dplyr::n())  %>%  dplyr::ungroup() %>%
      dplyr::mutate(fish_id2=factor(fish_id2))

    stom[['PREY']] <- dplyr::left_join(stom[['PREY']],dplyr::select(stom[['PRED']],sample_id,fish_id,sample_id2,fish_id2),by = c("sample_id", "fish_id")) %>%
      dplyr::mutate(sample_id=NULL,fish_id=NULL) %>% rename(sample_id=sample_id2,fish_id=fish_id2)
    stom[['PRED']] <- stom[['PRED']] %>% dplyr::mutate(sample_id=NULL,fish_id=NULL) %>% rename(sample_id=sample_id2,fish_id=fish_id2)
  }

  # reorganize_fields)
  stomach_format<- eval(control@stomach_format)
  b <- read.csv(file = stomach_format, stringsAsFactors = FALSE)
  for (pp in c('PRED','PREY')) {
     if (pp=='PRED') {field <- subset(b,PRED==TRUE)$field; print(field)}
     if (pp=='PREY') {field <- subset(b,PREY==TRUE)$field ;print(field)}
     fields<-data.frame(field=field,n=1:length(field))
     cols<-colnames(stom[[pp]])
     incl<-intersect(cols,field)
     fields<-subset(fields,field %in% incl)
     fields<-fields[order(fields$n),]
     incl<-as.character(fields$field)
     stom[[pp]]<- stom[[pp]] %>% dplyr::select(dplyr::all_of(incl))
  }

  if (control_criteria) {
    stom<-subset(stom,year %in% control@years)
    if ("quarter" %in% colnames(stom[['PRED']])) stom<-subset(stom,quarter %in% control@quarters)
  }
  attr(stom,all_stom_attributes()["changes"])<-TRUE
  return(stom)
}
