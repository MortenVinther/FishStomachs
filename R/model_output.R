#' Model output for the SMS model are produced from a STOMdiet object.
#' @title Make model output
#' @param diet Object of class STOMdiet.
#' @param length_classes_file bla
#' @param sp_info_file bla
#' @param intoSMSformat bla
#' @param  pred_is_coded  Logical; predator name is already on a short form (e.g. FAO 3 letter code)
#' @param  prey_is_coded  Logical; prey name is already on a short form (e.g. FAO 3 letter code)
#' @return Stomach contents data of class STOMobs.
#' @importFrom dplyr mutate
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr if_else
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @export
#' @examples \dontrun{x=2}
#'
model_output<-function(diet,  length_classes_file, sp_info_file,intoSMSformat=FALSE, pred_is_coded =FALSE, prey_is_coded =FALSE) {

 # test diet<-d2; length_classes_file=file.path(config_dir,paste0("length_classes_",sz,".csv")) ; sp_info_file<-file.path(config_dir,'species_info.csv')
  a<-area<-b<-code<-config_dir<-group<-key<-mean.weight<-n_tot<-no<-number<-pred_id_number<-pred_l_mean<-pred_name<-NULL
  pred_size<-pred_size_class<-predator<-prey<-prey_id_number<-prey_l_mean<-prey_name<-prey_size<-prey_size_class<-NULL
  prey_w<-quarter<-sampling_effort<-sl<-SMS_species<-Species<-stomcon<-stratum_area<-stratum_time<-type<-year<-NULL
  group<-NULL

  if (missing(length_classes_file)) stop('Parameter length_classes_file is missing, please provide filename for information on length classes, See function make_length_classess()\n')

  control<-attr(diet,'control')
  max_l<-control@max_l

  #check that the right options are available
  nc<-names(control@model_options)
  required<-c("minimum_stom","insert_mid_value","mid_value","insert_tails","tail_value")

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

  lprey <-  read_csv(length_classes_file,col_types = cols()) %>%
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
  sp_info<- read_csv(file= sp_info_file,col_types = cols()) %>% dplyr::mutate(number=as.integer(number))

  if (pred_is_coded) {
    bb<- bb %>% dplyr::mutate(predator=pred_name,pred_name=NULL)
  } else {
    bb<-dplyr::left_join(bb,dplyr::select(sp_info,predator=code,pred_name=SMS_species),by = "pred_name") %>% mutate(pred_name=NULL)
  }
  bb<-dplyr::left_join(bb,dplyr::select(sp_info,pred_id_number=number,predator=code),by = "predator")

  if (prey_is_coded) {
    bb<- bb %>% dplyr::mutate(prey=prey_name,prey_name=NULL)
  } else {
    bb<-dplyr::left_join(bb,dplyr::select(sp_info,prey=code,prey_name=SMS_species),by = "prey_name") %>% mutate(prey_name=NULL)
  }
  bb<-dplyr::left_join(bb,dplyr::select(sp_info,prey_id_number=number,prey=code),by = "prey")

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
      type=type) %>% arrange(area,year,quarter,pred_id_number,pred_size_class,prey_id_number, prey_size_class)

   if (intoSMSformat) {

     bb<- dplyr::left_join(bb,dplyr::select(sp_info,prey_id_number=number,lw_a,lw_b),by = "prey_id_number")

     bb<- bb %>% mutate(mean.weight=if_else(lw_a<0,9999,lw_a*prey_l_mean**lw_b),lw_a=NULL,lw_b=NULL)
     bb<-bb %>% transmute(SMS_area=unclass(factor(area)),year=year,quarter=quarter,pred=predator,pred.no=pred_id_number,
                               pred.size=substr(pred_size,1,4),pred.size.class=pred_size_class,
                               pred.mean.length=pred_l_mean,prey=prey,prey.no=prey_id_number,
                               prey.size=substr(prey_size,1,4),prey.size.range=prey_size,prey.size.class=prey_size_class,
                               prey.mean.length=prey_l_mean,prey.mean.length.ALK=-9,
                               stomcon=prey_w,type,mean.weight=mean.weight,haul.no=sampling_effort,
                               haul.prey.no=1,calc.prey.number=-1,used.prey.number=-1) %>%
                 filter(!(is.na(stomcon)))
   }

  return(bb)
}

