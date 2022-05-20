#' Transform diet data (of class STOMdiet) into data used for the SMS model.
#' @title  Produce input for the SMS model.
#'
#' @param diet Object of class STOMdiet.
#' The transformation uses values provided by the list \code{control@model_options}
#' The list must include the names
#' \preformatted{
#'   "delete_small": logical, delete input with relative stomach contents weight lower than "min_value".
#'                 If TRUE values smaller than "min_values" are deleted.
#'                 If FALSE values lower than "min_value" are increased to "min_value".
#'   "min_value": minimum relative stomach contents weight (see above)
#'   "insert_mid_value": logical, insert dummy value ("mid_value") where observations for a prey
#'                 within a size range are missing
#'   "mid_value": value used for missing mid-values.
#'   "insert_tails", logical for insertion of dummy values("tail_value") for the prey size class
#'                which are lower and higher than the observed size range for a prey
#'   "tail_value":  value used for missing tail-values.
#' }
#'  In addition a number for each predator and prey species is added (and later used for ordering of species).
#'  data for that is included in the CSV file sp_info_file.
#'
#' @param length_classes_file bla
#' @param sp_info_file CSV file with the fields used arranging order of predator and prey.
#' The file must include two fields \code{code} (predator and prey names used in input)
#' and \code{number} (integer for arranging predator and preys).
#' @param intoSMSformat Logical for transformation into SMS format.
#' @return  Data for use in the SMS model.
#' @export
#' @examples \dontrun{x=2}
#'
model_output<-function(diet,  length_classes_file, sp_info_file,intoSMSformat=FALSE) {

 # test diet<-d; length_classes_file=file.path(config_dir,paste0("length_classes_",sz,".csv")) ; sp_info_file<-file.path(config_dir,'species_info.csv')
  a<-area<-b<-code<-config_dir<-group<-key<-mean.weight<-n_tot<-no<-number<-pred_id_number<-pred_l_mean<-pred_name<-NULL
  pred_size<-pred_size_class<-predator<-prey<-prey_id_number<-prey_l_mean<-prey_name<-prey_size<-prey_size_class<-NULL
  prey_w<-quarter<-sampling_effort<-sl<-SMS_species<-Species<-stomcon<-stratum_area<-stratum_time<-type<-year<-NULL
  group<-NULL

  if (missing(length_classes_file)) stop('Parameter length_classes_file is missing, please provide filename for information on length classes, See function make_length_classess()\n')

  control<-get_control(diet)
  max_l<-control@max_l

  #check that the right options are available
  nc<-names(control@model_options)

  required<-names(new("STOMcontrol")@model_options)

  if (!all(required %in% nc)) stop('The control@model_options list does not include the names: ',paste0(required,collapse='; '),
      '\n but includes: ',paste(nc,collapse='; '))

  # calculate year and quarter from specifications
  diet[['PRED']]<-diet[['PRED']] %>% dplyr::mutate(year=as.integer(eval(control@strata_year_back)),quarter=as.integer(eval(control@strata_quarter_back)))

  p<-dplyr::inner_join(diet[['PRED']],diet[['PREY']],by = "key") %>% dplyr::select(-key)


  minl<-min(p$prey_size_class);maxl<-max(p$prey_size_class)

  prey_range<- p %>%  dplyr::group_by (stratum_time,year,quarter,stratum_area,pred_name,pred_size,pred_size_class,prey_name) %>%
    dplyr::summarise(minl=min(prey_size_class),maxl=max(prey_size_class))


  b_expand<-p %>% tidyr::complete(tidyr::nesting(stratum_time,year,quarter,stratum_area,pred_name,pred_size,pred_size_class, n_tot, pred_l_mean,prey_name), prey_size_class)

  bb<-dplyr::left_join(b_expand,prey_range,by = c("stratum_time", "year", "quarter", "stratum_area", "pred_name", "pred_size", "pred_size_class", "prey_name")) %>%
    dplyr::filter(prey_size_class>=minl-1 & prey_size_class<=maxl+1) %>%
    dplyr::mutate(type=dplyr::if_else(!is.na(prey_w),'obs',dplyr::if_else(prey_size_class>=minl & prey_size_class<=maxl,'mid','tail'))) %>%
    dplyr::filter(!(prey_name=='other' & prey_size_class !=0))  %>%
    dplyr::mutate(minl=NULL,maxl=NULL)

  # put missing prey_size on

  lprey <-  read_csv(length_classes_file,col_types = readr::cols()) %>%
      dplyr::mutate(l1=NULL,l2=NULL) %>%
      dplyr::mutate_if(is.numeric,as.integer) %>%
      dplyr::rename(prey_name=Species,prey_size_class=no,sl=group)


  if (any(lprey$prey_name=='ALL')) lprey<-dplyr::mutate(lprey,prey_name=NULL)
  bb<-dplyr::left_join(bb,lprey,by = c("year", "quarter", "prey_size_class")) %>%
    dplyr::mutate(prey_size=dplyr::if_else(is.na(prey_size),factor(sl,levels=levels(bb$prey_size)),prey_size)) %>%
    dplyr::filter(!is.na(prey_size)) %>% dplyr::mutate(sl=NULL)
  bb[bb$type=='mid','prey_w']<-control@model_options$mid_value
  bb[bb$type=='tail','prey_w']<-control@model_options$tail_value

  if (!control@model_options$insert_tails) bb<- bb %>% dplyr::filter(type != 'tail')

  bb <- dplyr::select(bb,year,quarter,stratum_time, stratum_area, pred_name, pred_size,pred_size_class,pred_l_mean,n_tot, prey_name,type, prey_size,prey_size_class, prey_w) %>%
    dplyr::group_by(year,quarter,stratum_time, stratum_area, pred_name, pred_size,pred_size_class) %>%
    dplyr::mutate(prey_w=prey_w/sum(prey_w)) %>% dplyr::ungroup()
  bb<- dplyr::mutate_if(bb, is.factor,as.character)

  # add additional information
  sp_info<- read_csv(file= sp_info_file,col_types = readr::cols()) %>% dplyr::mutate(number=as.integer(number))


  bb<- bb %>% dplyr::mutate(predator=pred_name,pred_name=NULL)
  bb<-dplyr::left_join(bb,dplyr::select(sp_info,pred_id_number=number,predator=SMS_species),by = "predator")


  bb<- bb %>% dplyr::mutate(prey=prey_name,prey_name=NULL)
  bb<-dplyr::left_join(bb,dplyr::select(sp_info,prey_id_number=number,prey=SMS_species),by = "prey")

  bb<-dplyr::mutate(bb,prey_l_mean=rowMeans(matrix(as.numeric(unlist(strsplit(prey_size,split='-'))),ncol=2,byrow=TRUE)))


  bb<-bb %>% dplyr::select(
      area=stratum_area,
      year,
      quarter,
      predator,
      pred_id_number,
      pred_size,
      pred_size_class,
      pred_l_mean,
      sampling_effort=n_tot,
      prey,
      prey_id_number,
      prey_size,
      prey_size_class,
      prey_l_mean,
      prey_w,
      type=type) %>% dplyr::arrange(area,year,quarter,pred_id_number,pred_size_class,prey_id_number, prey_size_class)

   if (intoSMSformat) {

     bb<- dplyr::left_join(bb,dplyr::select(sp_info,prey_id_number=number,lw_a,lw_b),by = "prey_id_number")

     bb<- bb %>% dplyr::mutate(mean.weight=if_else(lw_a<0,9999,lw_a*prey_l_mean**lw_b),lw_a=NULL,lw_b=NULL)
     bb<-bb %>% dplyr::transmute(SMS_area=unclass(factor(area)),year=year,quarter=quarter,pred=predator,pred.no=pred_id_number,
                               pred.size=substr(pred_size,1,4),pred.size.class=pred_size_class,
                               pred.mean.length=pred_l_mean,prey=prey,prey.no=prey_id_number,
                               prey.size=substr(prey_size,1,4),prey.size.range=prey_size,prey.size.class=prey_size_class,
                               prey.mean.length=prey_l_mean,prey.mean.length.ALK=-9,
                               stomcon=prey_w,type,mean.weight=mean.weight,haul.no=sampling_effort,
                               haul.prey.no=1,calc.prey.number=-1,used.prey.number=-1) %>%
                 dplyr::filter(!(is.na(stomcon)))
   }

  return(bb)
}

