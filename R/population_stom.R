#' Check if data are ready for aggregation.
#'
#' @title Check if data are ready for aggregation.
#' @param stom Stomach data set of class STOMobs.
#' @return logical
#' @export
#' @examples \dontrun{check_before_strata_aggregation(stom)}
check_before_strata_aggregation<-function(stom) {

  control<-attr(stom,'control')
  mis_l <- control@mis_l
  other<-control@other  # id for "other food"
  mis_ll<-paste(mis_l,mis_l,sep='-')

  if (!attr(stom,"Temporal and spatial strata added")) stop('Spatial and temporal strata variables have not been addde. Please use "add_strata"')

  tst<-subset(stom,prey_name %in% control@sel_preys,prey_size ==mis_ll)
  if (dim(tst[['PRED']])[[1]]>0) {
    cat('ERROR: there are missing length informations for the preys:',paste(control@sel_preys,collapse=", "))
    print(tst)
    invisible(tst)
  }
  tst<-subset(stom,prey_name %in% control@sel_preys,is.na(prey_w))
  if (dim(tst[['PRED']])[[1]]>0) {
    cat('ERROR: there are NA for prey weight for the preys:',paste(control@sel_preys,collapse=", "))
    print(tst)
    invisible(tst)
  }

  invisible(TRUE)
}


refac_prey<-function(d){
  oth<-attr(d,'control')@other
  d[['PREY']]$prey_name<-droplevels(d[['PREY']]$prey_name)
  pn<-levels(d[['PREY']]$prey_name)
  pn<-c(pn[!grepl(oth,pn)],oth)
  d[['PREY']]$prey_name<-forcats::fct_relevel(d[['PREY']]$prey_name,pn)
  return(d)
}

#' Make a template for weighting factors.
#'
#' @title Make a template for weighting factors for population diet.
#' @param stom Stomach data set of class STOMobs.
#' @param strata Level for stratification.  One of 'sub_strata', 'strata' or 'total'
#' @param write_CSV Logical for write of a CSV file in the
#' @return logical
#' @export
#' @examples \dontrun{make_template_strata_weighting(stom)}
make_template_strata_weighting<-function(stom,strata=c('sub_strata','strata','total')[1],write_CSV=FALSE){
  # check that strata are added
  if (!attr(stom,"Temporal and spatial strata added")) stop('Spatial and temporal strata variables have not been addde. Please use "add_strata"')
  control<-attr(stom,'control')

  if (strata=='sub_strata') file_n<-control@calc_sub_strata$weigthing_factor_file
  if (strata=='strata') file_n<-control@calc_strata$weigthing_factor_file
  if (strata=='total')  file_n<-control@calc_total$weigthing_factor_file
  if (is.na(file_n)) {
     cont<-c("calc_sub_strata","calc_sub_strata","calc_total") ;names(cont)<-  c('sub_strata','strata','total')
    stop('A file name must be given in the control slot ',cont[strata], ' variable weighting_factor_file \n')
  }
   st_names<-c("pred_name","stratum_time","pred_size", "stratum_area","stratum_sub_area","sample_id")
   if (strata=='strata') {st_names <-head(st_names,-1);  file_n<-control@calc_strata$weigthing_factor_file }
   if (strata=='total')  {st_names <-head(st_names,-2) ; file_n<-control@calc_total$weigthing_factor_file}
   stom <- stom[['PRED']] %>% dplyr::select(dplyr::all_of(st_names)) %>% dplyr::distinct()

  if (strata=='sub_strata') stom<-dplyr::mutate(stom,w_fac_sample=1)
  if (strata=='strata') stom<-dplyr::mutate(stom,w_fac_sub_area=1)
  if (strata=='total') stom<-dplyr::mutate(stom,w_fac_area=1)
  stom<- stom %>% dplyr::mutate_if(is.factor,as.character)
  if (write_CSV) {
    write_csv(stom,file=file_n)
    cat('File: ',file_n,'has been written. Please edit weighting factors.\n')
  }
  invisible(stom)
}


read_strata_weighting<-function(stom,strata=c('sub_strata','strata','total')[1]){
  # check that strata are added

  if (!attr(stom,"Temporal and spatial strata added")) stop('Spatial and temporal strata variables have not beeb addde. Please use "add_strata"')
  control<-attr(stom,'control')

  if (strata=='sub_strata') file_n<-control@calc_sub_strata$weigthing_factor_file
  if (strata=='strata') file_n<-control@calc_strata$weigthing_factor_file
  if (strata=='total')  file_n<-control@calc_total$weigthing_factor_file
  w<-read_csv(file=file_n,progress = FALSE,col_types = readr::cols()) %>% dplyr::distinct()
  return(w)
}



#' Compilation of observed stomach contents into population diet
#'
#' Stratification of data must first be done by the function \code{\link{add_strata}}.
#' With stratification added diet data are firstly compiled for each sub_strata (e.g. ICES rectangles),
#' then compiled by strata (e.g. ICES roundfish areas)
#' and finally compiled for the whole population area.
#' The methods for data compilations must be provided in the control object to the input data, see Details.
#'
#' The compilation method must be specified in the control attribute by the slots.
#' \code{@calc_sub_strata, @calc_strata} and \code{@calc_total}.
#' Each of the slots must include a list
#' with the following names \code{relative_weight} (type=boolean), \code{weighting_factor} (type=expression)
#' and \code{weigthing_factor_file} (type character). An example of such input is shown below:
#'
#'\preformatted{
#' hom<-edit_control(hom,
#'  calc_sub_strata=list(
#'     # do not transform into relative weight before data are compiled
#'    relative_weight=FALSE,
#'     # use number of stomachs (n_tot) as weighting factor
#'    weighting_factor=expression(n_tot),
#'     # do not use data from external file
#'    weigthing_factor_file=NA
#'  ),
#'  calc_strata=list(
#'    # weight mean stomach contents by sub_strata by the square root of the mean CPUE within the sub_strata
#'   relative_weight=TRUE,
#'   weighting_factor=expression(sqrt(pred_cpue)),
#'   weigthing_factor_file=NA
#'  )
#'  calc_total=list(
#'    relative_weight=TRUE,
#'    weighting_factor=NA,
#'    weigthing_factor_file=file.path(config_dir,'hom_weighting_total.csv')
#'))
#'}
#'
#' The \code{calc_sub_strata} list specifies that the observed stomach contents by samples should not be translated
#' into a relative stomach contents by sample (relative_weight=FALSE) before processed. In cases with more samples from a
#' sub_strata, the average stomach contents within the sub_strata should be calculated as a weighted mean where the number of stomachs
#' sampled (n_tot) is used as weighting factor. Note that you have to use "expression" if you refer to a variable (in this case, n_tot)
#' withinh the data set.
#'
#' The \code{calc_strata} list specified that the average stomach contents by sub_strata initially should be transformed
#' into relative stomach contents by sub_strata (relative_weight=TRUE). The average stomach contents within a stratum
#' should the be calculated as a weighted mean using the squaret root of the CPUE as weighting factor.
#'
#' The average stomach contents, specified by the list \code{calc_total} should be calculated as a weighed mean of
#' the relative stomach contents by stratum weighted by a factor provided in the file hom_weighting_total.csv.
#' Some rows from the file are shown below:
#'
#' \preformatted{
#' pred_name, stratum_time, pred_size, stratum_area, w_fac_area
#' HOM,       1991-Q3,      0250-0300,  R-2,         25
#' HOM,       1991-Q3,      0350-0400,  R-2,          2
#' HOM,       1991-Q3,      0300-0350,  R-2,         25
#' HOM,       1991-Q4,      0250-0300,  R-3,         19
#' HOM,       1991-Q4,      0350-0400,  R-1,         45
#' }
#' The file specifies which weighting factor, \code{w_fac_area}, that should be used for a given
#' predator and predator size, within a given temporal (time) stratum and area stratum. See
#' \code{\link{make_template_strata_weighting}} for how to make such file with the right format.
#'
#' @param s Stomach data set of class STOMobs. It is assumed that the stomach contents weight correspond to the
#' number of stomachs, i.e. the stomach contents for a single predator, or the sum of preys if the sample includes more than one
#' predator (stomach).
#' @param verbose Logical, show details
#'
#' @return Diet data set of class STOMdiet
#' @export
#'
#'
calc_population_stom<-function(s,verbose=FALSE) {

  fish_id<-key<-mean_cpue<-n_tot<-pred_cpue<-pred_l_mean<-pred_name<-pred_size<-pred_size_class<-prey_name<-prey_size<-prey_size_class<-prey_w<-sample_id<-stratum_area<-stratum_sub_area<-stratum_time<-sum_lw<-sum_w<-sum_w_fac<-w_fac_area<-w_fac_sample<-w_fac_sub_area<-NULL
  options(dplyr.summarise.inform = FALSE)
  stopifnot(check_before_strata_aggregation(s))
  control<-attr(s,'control')
  min_prey_length<-control@min_prey_length
  mis_l<-control@mis_l
  other<-control@other
  mis_size_class<-control@mis_size_class
  mis_ll<-paste(mis_l,mis_l,sep='-')
  do_details <- control@detailed_tst_output

  check_sc<-function(sc,strata=c('sub_strata','strata','total')[1]){
    sc_names<-names(sc)
    if (!("relative_weight" %in% sc_names & "weighting_factor" %in% sc_names &"weigthing_factor_file" %in% sc_names)) {
    cont<-c("calc_sub_strata","calc_sub_strata","calc_total") ;names(cont)<-  c('sub_strata','strata','total')
      stop(cat('The control slot',cont[strata],' must include a list with the names: relative_weight, weighting_factor and weigthing_factor_file.\n',
               'It includes now the names:', paste(sc_names,collapse=', '),'\n' ))
    }
  }


  empty_stom<-function(x){
    crit<-is.na(x$prey_name)
    x[crit,'prey_name']<-other
    x[crit,'prey_size']<-mis_ll
    x[crit,'prey_size_class']<-mis_size_class
    x[crit,'prey_w']<-0
    return(x)
  }

  if (do_details) do_detailed_output(s,append=FALSE,label='NEW set: Before any change',digits=1,rel_weight=FALSE,write_criteria=TRUE,show_mis_length=FALSE,transpose=TRUE,by_sample_id=TRUE)

  pred<-s[['PRED']]  %>% dplyr::mutate_if(is.factor,as.character)
  prey<-s[['PREY']]  %>% dplyr::mutate_if(is.factor,as.character)

  n_samples<-pred %>% dplyr::group_by(stratum_time, pred_name,pred_size) %>% dplyr::summarize(n_sample=n()) %>% dplyr::ungroup()

  sc<-control@calc_sub_strata
  check_sc(sc,strata=c('sub_strata','strata','total')[1])
  if (!is.na(sc$weigthing_factor_file)) {
    w<-read_strata_weighting(stom=s,strata=c('sub_strata','strata','total')[1])
    pred<-dplyr::left_join(pred,w, by = c("sample_id", "pred_name", "pred_size", "stratum_time", "stratum_area", "stratum_sub_area"))  %>%
      dplyr::filter(!is.na(w_fac_sample)) %>% dplyr::filter(w_fac_sample>0)
    if (verbose) cat('Using weighting factors from file',sc$weigthing_factor_file,'\n')
  } else {
     tt<-try(pred<-pred %>% dplyr::mutate(w_fac_sample=eval(control@calc_sub_strata$weighting_factor)),TRUE)
     if (class(tt)[[1]]=="try-error") {cat(tt[1]);stop('Error found.')}
  }


  if (!any(grepl("w_fac_sample",names(pred)))) pred<-dplyr::mutate(pred,w_fac_sample=1)
  if (!any(grepl("pred_cpue",names(pred)))) pred<-dplyr::mutate(pred,pred_cpue=1)

  pred <-dplyr::select(pred,stratum_time, stratum_area, stratum_sub_area, pred_name, pred_size_class, pred_size,sample_id, fish_id, n_tot, pred_l_mean, pred_cpue, w_fac_sample)

    #contents per stomach and include empty stomachs
  prey<-dplyr::left_join(pred,prey, by = c("sample_id", "fish_id"))
  prey<-empty_stom(prey)  %>% dplyr::mutate(prey_w=prey_w/n_tot)

  prey<- dplyr::group_by(prey,stratum_sub_area, stratum_area, stratum_time,pred_name,pred_size,w_fac_sample,sample_id, fish_id, prey_name, prey_size_class,prey_size) %>%
    dplyr::summarise(prey_w=sum(prey_w))

  if (sc$relative_weight){
    prey<-dplyr::group_by(prey,stratum_sub_area, stratum_area, stratum_time,pred_name,pred_size, w_fac_sample,sample_id, fish_id)  %>%
      dplyr::mutate(prey_w=prey_w/sum(prey_w))
    prey<-empty_stom(prey)
  }

  if (do_details) {
    PRED <- pred %>% dplyr::mutate_if(is.factor,as.character) %>% dplyr::mutate(sample_id=factor(sample_id),fish_id=factor(fish_id))
    PREY<- dplyr::ungroup(prey) %>% dplyr::select(sample_id ,fish_id,prey_name, prey_size_class, prey_size,  prey_w) %>%
           dplyr::mutate_if(is.factor,as.character) %>%
           dplyr::mutate(sample_id=factor(sample_id,levels=levels(PRED$sample_id)), fish_id=factor(fish_id,levels=levels(PRED$fish_id)))
    a <- list(PRED=PRED, PREY = PREY)
    class(a) <- "STOMobs"
    attr(a,'control')<-control
    do_detailed_output(a,append=TRUE,label='NEW set: after stomach content per stomach and potential relative weight',digits=1,rel_weight=control@calc_sub_strata$relative_weight,write_criteria=FALSE,show_mis_length=FALSE,transpose=TRUE,by_sample_id=TRUE)
  }

  # weighted mean prey_w by stratum_sub_area
  prey<- prey %>% dplyr::mutate(prey_w=prey_w*w_fac_sample) %>% dplyr::group_by(stratum_sub_area,stratum_area, stratum_time,pred_name,pred_size,prey_name,prey_size,prey_size_class)  %>%
    dplyr::summarise(sum_w=sum(prey_w))
  fac<- pred %>%  dplyr::group_by( stratum_sub_area,stratum_area, stratum_time,pred_name,pred_size) %>% dplyr::summarise(w_fac_sample=sum(w_fac_sample))

  prey<-dplyr::left_join(prey,fac,by = c("stratum_sub_area", "stratum_area", "stratum_time", "pred_name", "pred_size")) %>%
   dplyr::mutate(prey_w=sum_w/w_fac_sample, sum_w=NULL,sum_w_fac=NULL,w_fac_sample=NULL)

  # sum n_tot and weighted mean of predator mean length and mean CPUE
  pred<- pred %>% dplyr::group_by(stratum_sub_area,stratum_area, stratum_time,pred_name,pred_size,pred_size_class) %>%
      dplyr::summarise(n_tot=sum(n_tot),sum_lw=sum(pred_l_mean* w_fac_sample),sum_w_fac=sum(w_fac_sample),mean_cpue=mean(pred_cpue,na.rm=TRUE)) %>%
      dplyr::mutate(pred_l_mean=sum_lw/sum_w_fac,sum_w_fac=NULL,sum_lw=NULL)

  if (do_details) {
    PRED <- pred %>% dplyr::mutate(sample_id=dplyr::cur_group_id(),fish_id='1')  %>% dplyr::ungroup() %>%
            dplyr::mutate(sample_id=factor(sample_id),fish_id=factor(fish_id))
    PREY<- dplyr::ungroup(prey) %>%
      dplyr::select(stratum_sub_area,stratum_area, stratum_time,pred_name,pred_size,prey_name, prey_size_class, prey_size,  prey_w)

    PREY<-dplyr::left_join(PREY,PRED,by = c("stratum_sub_area", "stratum_area", "stratum_time", "pred_name", "pred_size"))   %>%
      dplyr::select(sample_id,fish_id,prey_name, prey_size_class, prey_size,  prey_w)
     a <- list(PRED=PRED, PREY = PREY)
    class(a) <- "STOMobs"
    attr(a,'control')<-control
    do_detailed_output(a,append=TRUE,label='NEW set: after aggregation by sub-area strata. Average stomach contents by sub-strata.',digits=1,rel_weight=control@calc_sub_strata$relative_weight,write_criteria=FALSE,show_mis_length=FALSE,transpose=TRUE,by_sample_id=TRUE)
  }

  #############  by strata area
  sc<-control@calc_strata
  check_sc(sc,strata=c('sub_strata','strata','total')[2])
  if (!is.na(sc$weigthing_factor_file)) {
    w<-read_strata_weighting(stom=s,strata=c('sub_strata','strata','total')[2])
    pred<-dplyr::left_join(pred,w,by = c("stratum_sub_area", "stratum_area", "stratum_time", "pred_name", "pred_size"))  %>%
      dplyr::filter(!is.na(w_fac_sub_area)) %>% dplyr::filter(w_fac_sub_area>0)
    cat('Using area weighting factors from file',sc$weigthing_factor_file,'\n')
  } else {
    tt<-try(pred<-pred %>% dplyr::mutate(w_fac_sub_area=eval(control@calc_strata$weighting_factor)),TRUE)
    if (class(tt)[[1]]=="try-error") {cat(tt[1]);stop('Error found.')}
  }

  prey<-dplyr::left_join(pred,prey, by = c("stratum_sub_area", "stratum_area", "stratum_time", "pred_name", "pred_size"))

  if (sc$relative_weight){
    prey<-dplyr::group_by(prey,stratum_sub_area, stratum_area, stratum_time,pred_name,pred_size, w_fac_sub_area)  %>%
      dplyr::mutate(prey_w=prey_w/sum(prey_w)) %>% dplyr::ungroup() %>% dplyr::filter(!is.na(prey_w))
    prey<-empty_stom(prey)
  }

  # weighted mean prey_w by stratum_area
  prey<- prey %>% dplyr::mutate(prey_w=prey_w*w_fac_sub_area) %>% dplyr::group_by(stratum_area,stratum_time,pred_name,pred_size,prey_name,prey_size,prey_size_class)  %>%
    dplyr::summarise(prey_w=sum(prey_w))
  fac<- pred %>%  dplyr::group_by( stratum_area, stratum_time,pred_name,pred_size) %>% dplyr::summarise(w_fac_sub_area=sum(w_fac_sub_area))
  prey<-dplyr::left_join(prey,fac,by = c( "stratum_area", "stratum_time", "pred_name", "pred_size")) %>%
    dplyr::mutate(prey_w=prey_w/w_fac_sub_area, w_fac_sub_area=NULL)

  # sum n_tot and weighted mean of predator mean length and mean CPUE
  pred<- pred %>% dplyr::group_by(stratum_area, stratum_time,pred_name,pred_size,pred_size_class) %>%
    dplyr::summarise(n_tot=sum(n_tot),sum_lw=sum(pred_l_mean* w_fac_sub_area),sum_w_fac=sum(w_fac_sub_area),mean_cpue=mean(mean_cpue,na.rm=TRUE)) %>%
    dplyr::mutate(pred_l_mean=sum_lw/sum_w_fac,sum_w_fac=NULL,sum_lw=NULL)

  if (do_details) {
    PRED <- pred %>% dplyr::mutate(sample_id=dplyr::cur_group_id(),fish_id='1')  %>% dplyr::ungroup() %>%
      dplyr::mutate(sample_id=factor(sample_id),fish_id=factor(fish_id))
    PREY<- dplyr::ungroup(prey) %>%
      dplyr::select(stratum_area, stratum_time,pred_name,pred_size,prey_name, prey_size_class, prey_size, prey_w)
    PREY<-dplyr::left_join(PREY,PRED,by = c("stratum_area", "stratum_time", "pred_name", "pred_size"))   %>%
      dplyr::select(sample_id,fish_id,prey_name, prey_size_class, prey_size,  prey_w)
    a <- list(PRED=PRED, PREY = PREY)
    class(a) <- "STOMobs"
    attr(a,'control')<-control
    do_detailed_output(a,append=TRUE,label='NEW set: after aggregation by area strata.',digits=1,rel_weight=control@calc_strata$relative_weight,write_criteria=FALSE,show_mis_length=FALSE,transpose=TRUE,by_sample_id=TRUE)
  }

  ### Total
  sc<-control@calc_total
  check_sc(sc,strata=c('sub_strata','strata','total')[3])
  if (!is.na(sc$weigthing_factor_file)) {
    w<-read_strata_weighting(stom=s,strata=c('sub_strata','strata','total')[3])
    pred<-dplyr::left_join(pred,w ,by = c("stratum_area", "stratum_time", "pred_name", "pred_size")) %>%
      dplyr::filter(!is.na(w_fac_area)) %>% dplyr::filter(w_fac_area>0)
    if (verbose) cat('Using weighting factors from file',sc$weigthing_factor_file,'\n')
  } else {
    tt<-try(pred<-pred %>% dplyr::mutate(w_fac_area=eval(control@calc_total$weighting_factor)),TRUE)
    if (class(tt)[[1]]=="try-error") {cat(tt[1]);stop('Error found.')}
  }

  prey<-dplyr::left_join(pred,prey,by = c("stratum_area", "stratum_time", "pred_name", "pred_size"))

  if (sc$relative_weight){
    prey <- dplyr::group_by(prey, stratum_area, stratum_time,pred_name,pred_size, w_fac_area)  %>%
      dplyr::mutate(prey_w=prey_w/sum(prey_w)) %>% dplyr::ungroup() %>% dplyr::filter(!is.na(prey_w))
    prey<-empty_stom(prey)
  }

  # weighted mean prey_w by area
  # dplyr::filter(prey,stratum_time=='1981-Q3')
  prey<- prey %>% dplyr::mutate(prey_w=prey_w*w_fac_area) %>% dplyr::group_by(stratum_time,pred_name,pred_size,prey_name,prey_size,prey_size_class)  %>%
    dplyr::summarise(prey_w=sum(prey_w))
  fac<- pred %>%  dplyr::group_by( stratum_time,pred_name,pred_size) %>% dplyr::summarise(w_fac_area=sum(w_fac_area))
  prey<-dplyr::left_join(prey,fac,by = c("stratum_time", "pred_name", "pred_size")) %>%
    dplyr::mutate(prey_w=prey_w/w_fac_area, w_fac_area=NULL,stratum_area='all')

  # sum n_tot and weighted mean of predator mean length
  pred<- pred %>% dplyr::group_by( stratum_time,pred_name,pred_size,pred_size_class) %>%
    dplyr::summarise(n_tot=sum(n_tot),sum_lw=sum(pred_l_mean* w_fac_area),sum_w_fac=sum(w_fac_area)) %>%
    dplyr::mutate(pred_l_mean=sum_lw/sum_w_fac,sum_w_fac=NULL,sum_lw=NULL,stratum_area='all')


  if (do_details) {
    PRED <- pred %>% dplyr::mutate(sample_id=dplyr::cur_group_id(),fish_id='1')  %>% dplyr::ungroup() %>%
      dplyr::mutate(sample_id=factor(sample_id),fish_id=factor(fish_id))
    PREY<- dplyr::ungroup(prey) %>%
      dplyr::select( stratum_time,pred_name,pred_size,prey_name, prey_size_class, prey_size, prey_w)
    PREY<-dplyr::left_join(PREY,PRED,by = c("stratum_time", "pred_name", "pred_size"))   %>%
      dplyr::select(sample_id,fish_id,prey_name, prey_size_class, prey_size,  prey_w)
    a <- list(PRED=PRED, PREY = PREY)
    class(a) <- "STOMobs"
    attr(a,'control')<-control
    do_detailed_output(a,append=TRUE,label='NEW set: after aggregation into total area.',digits=1,rel_weight=control@calc_total$relative_weight,write_criteria=FALSE,show_mis_length=FALSE,transpose=TRUE,by_sample_id=TRUE)
    do_detailed_output(a,append=TRUE,label='NEW set: after aggregation into total area.',digits=1,rel_weight=TRUE,write_criteria=FALSE,show_mis_length=FALSE,transpose=TRUE,by_sample_id=TRUE)
  }

  p<-dplyr::inner_join(pred,prey,by = c("stratum_time", "pred_name", "pred_size", "stratum_area")) %>% dplyr::ungroup() %>%
     dplyr::mutate(key=paste(stratum_area,pred_name,stratum_time,pred_size,sep='_')) %>% dplyr::filter(!is.na(prey_name))

  p<-left_join(p,n_samples,by = c("stratum_time", "pred_name", "pred_size"))

  p<- p %>% dplyr::mutate_if(is.character,as.factor)

  a <- list(PRED = dplyr::select(p,stratum_area,stratum_time,pred_name, pred_size, pred_size_class, n_tot, n_sample, pred_l_mean,key) %>% dplyr::distinct() %>% arrange(key) ,
            PREY = dplyr::select(p,key,prey_name, prey_size, prey_size_class, prey_w )  %>% arrange(key))


  class(a) <- "STOMdiet"
  attr(a,'control')<-control
  a<-refac_prey(a)
  return(a)
}

#' close open devices
#' @importFrom grDevices dev.list dev.off
cleanup<-function(){for(i in dev.list()) if (names(dev.off())=='null device') break()}


#' Plot diet data.
#'
#' @param d Diet data set of class STOMdiet.
#' @param cut_pred_size From to in substring of predator size
#' @param show_plot Show the resulting graphs on screen (or save the results for later processing)
#' @param addTitle Add predator name on top of the plot.
#' @param tAngle Angle X-axis text.
#' @param addNstom Show number of stomachs sampled
#' @param nstomAngle Angle number of stomachs text
#' @param Ncol number of columns in facet plot
#' @param Nrow number of rows in facet plot
#' @param Strip.position strip.position: "top" | "bottom" | "left" | "right"
#' @param Colours vector of colours for preys.
#' @param otherCol Colour for "other prey"
#' @param refac_prey Reorder preys
#' @return nothing (if show_plot=TRUE) or a list of plots.
#' @importFrom ggplot2 ggplot facet_wrap geom_col labs geom_text theme theme_minimal scale_fill_manual aes element_text element_line
#' @importFrom rlang .data
#' @method  plot STOMdiet
#' @export
plot.STOMdiet<-function(d,show_plot=TRUE,cut_pred_size=c(1,10),addTitle=FALSE,tAngle=90,addNstom=FALSE,nstomAngle=45,Ncol=2,Nrow=NULL,
                    Strip.position = c("top", "bottom", "left", "right")[1],Colours,otherCol='grey',refac_prey=FALSE) {
  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-quarter<-year<-NULL
  if (missing(Colours)) Colours<-c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )

  if (refac_prey) d<-refac_prey(d)
  Colours[nlevels(d[['PREY']]$prey_name)]<-otherCol
  pn<-levels(d[['PREY']]$prey_name)
  allNames<- Colours[1:length(pn)]
  names(allNames)<-pn

  control<-attr(d,'control')
  # calculate year and quarter from specifications
  x<-as.data.frame(d) %>% dplyr::mutate(year=as.integer(eval(control@strata_year_back)),quarter=as.integer(eval(control@strata_quarter_back)))

  x<-x %>%  dplyr::group_by(key) %>% dplyr::mutate(prey_w=prey_w/sum(prey_w)*100) %>% dplyr::ungroup() %>%
    dplyr::mutate(pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2])) %>%
    dplyr::group_by(stratum_time,pred_name, pred_size,n_tot,year,quarter,prey_name) %>% dplyr::summarise(prey_w=sum(prey_w)) %>%
    dplyr::ungroup() %>%
    dplyr::select(stratum_time,pred_name, pred_size,n_tot,year,quarter,prey_name,prey_w)


  out<-by(x,list(x$pred_name,x$year),function(x) {
    if (addTitle) tit<- as.character(x$pred_name)[1] else tit<-NULL
    a<-ggplot(x) +
       facet_wrap(~stratum_time,  ncol=Ncol, nrow=Nrow, strip.position = Strip.position)+
       scale_fill_manual(  values = allNames,name='Prey')+
       geom_col(aes(x=pred_size, y = prey_w, fill = prey_name ))+
       labs(x='Predator size',y='weight percentage',title=tit)+
      theme_minimal() +
      theme( panel.grid.major = element_line(linetype = "blank"),
             panel.grid.minor = element_line(linetype = "blank"),
             axis.text.x = element_text(angle = tAngle, vjust = 0.5),
             )

    if (addNstom) a<-a+geom_text(aes(pred_size, one, label = n_tot),
              vjust = 0.5, angle = nstomAngle, size=3,
              data = . %>% dplyr::select(pred_size,stratum_time,n_tot) %>% unique() %>%
                dplyr::mutate(one=15) %>% dplyr::arrange(stratum_time,pred_size))

    if (show_plot) print(a) else return(a)
  })
  if (show_plot) return() else return(out)
}

#' Plot diet data.
#'
#' @param d Diet data set of class STOMdiet.
#' @param cut_pred_size From to in substring of predator size
#' @param show_plot Show the resulting graphs on screen (or save the results for later processing)
#' @param addTitle Add predator name on top of the plot.
#' @param tAngle Angle X-axis text.
#' @param Colours vector of colours for preys.
#' @param otherCol Colour for "other prey"
#' @param refac_prey Reorder preys
#' @param byVar Make individual plots by combinations of 'year-quarter','year' or'quarter' or lump all data together ('none').
#' @return nothing (if show_plot=TRUE) or a list of plots.
#' @importFrom ggplot2 ggplot facet_grid geom_col labs geom_text theme theme_minimal scale_fill_manual aes element_text element_line
#' @importFrom rlang .data
#' @export
plotSize<-function(d,show_plot=TRUE,cut_pred_size=c(1,10),addTitle=FALSE,tAngle=90,
                   Colours,otherCol='grey',refac_prey=FALSE,
                   byVar=c('year-quarter','year','quarter','none')[1]) {

  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-quarter<-year<-NULL
  prey_name<-prey_size<-stratum_time<-NULL

  if (missing(Colours)) Colours<-c('red','green','plum','blue','cyan','yellow','coral','skyblue','purple','magenta','limegreen','pink' )

  if (refac_prey) d<-refac_prey(d)
  Colours[nlevels(d[['PREY']]$prey_name)]<-otherCol
  pn<-levels(d[['PREY']]$prey_name)
  allNames<- Colours[1:length(pn)]
  names(allNames)<-pn

  control<-attr(d,'control')
  # calculate year and quarter from specifications
  x<-as.data.frame(d) %>% dplyr::mutate(year=as.integer(eval(control@strata_year_back)),quarter=as.integer(eval(control@strata_quarter_back)))
  if (byVar=='year-quarter') {x$key<-paste(x$year,x$quarter,x$pred_name,x$pred_size); x$stratum_time<-paste(x$year,x$quarter,sep='_') }
  if (byVar=='year')         {x$key<-paste(x$year,x$pred_name,x$pred_size); x$stratum_time<-paste(x$year,sep='_') }
  if (byVar=='quarter')      {x$key<-paste(x$quarter,x$pred_name,x$pred_size); x$stratum_time<-paste(x$quarter,sep='_') }
  if (byVar=='none')         {x$key<-paste(x$pred_name,x$pred_size); x$stratum_time<-paste('all',sep='_') }
  x<- x %>%  dplyr::group_by(key) %>% dplyr::mutate(prey_w=prey_w/sum(prey_w)*100) %>% dplyr::ungroup() %>%
    dplyr::mutate(pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2]),
                  prey_size=substr(prey_size,cut_pred_size[1],cut_pred_size[2])) %>%
    dplyr::select(key,stratum_time,pred_name, pred_size,n_tot,prey_name,prey_size,prey_w)


  out<-by(x,list(x$pred_name,x$stratum_time),function(x) {
    if (addTitle) tit<- as.character(x$pred_name)[1] else tit<-NULL
    a<-ggplot(x) +
      facet_grid(vars(prey_name), vars(pred_size),scales = "free_y", drop = TRUE)+
      scale_fill_manual(  values = allNames,name='Prey')+
      geom_col(aes(x=prey_size, y = prey_w, fill = prey_name ))+
      labs(x='Prey size',y='weight percentage',title=tit)+
      theme(axis.text.x = element_text(angle = tAngle, vjust = 0.5 ),legend.position = 'none')
    if (show_plot) print(a) else return(a)
  })
  if (show_plot) return() else return(out)
}


#' Plot difference between two diet data set.
#'
#' @param d1 Diet data set of class STOMdiet.
#' @param d2 Diet data set of class STOMdiet.
#' @param relative Logical, relative weight difference is used if TRUE, else absolute weight difference
#' @param cut_pred_size From to in substring of predator size
#' @param cut_prey_size From to in substring of prey size
#' @param show_plot Show the resulting graphs on screen (or save the results for later processing)
#' @param addTitle Add predator name on top of the plot.
#' @param tAngle Angle X-axis text.
#' @param Colours Colours for positive and negative difference (d1-d2)
#' @param refac_prey Reorder preys
#' @param maxDif maximum difference to be shown
#' @param size_NA Size of missing observation
#' @param byVar Make individual plots by combinations of 'year-quarter','year' or 'quarter' or lump all data together ('none').
#' @return nothing (if show_plot=TRUE) or a list of plots.
#' @importFrom ggplot2 ggplot facet_grid geom_col labs theme  scale_color_manual aes element_text element_line geom_point
#' @export
plotdif<-function(d1,d2,relative=TRUE,show_plot=TRUE,cut_pred_size=c(1,10),cut_prey_size=c(1,10),addTitle=FALSE,tAngle=90,
                  Colours=c('green','red','blue'),refac_prey=FALSE,maxDif=5,size_NA=4,
                  byVar=c('year-quarter','year','quarter','none')[1]) {

   key<<-pred_name<-pred_size<-prey_w<-prey_w1<-prey_w2<-size_w<-col<-quarter<-year<-NULL
   dif_w<-dif_w2<-key<-pp<-prey_name<-prey_size<-stratum_time<-NULL

  #if (refac_prey) d<-refac_prey(d)
  control<-get_control(d1)
  x1<-as.data.frame(d1) %>%
    dplyr::mutate(year=as.integer(eval(control@strata_year_back)),quarter=as.integer(eval(control@strata_quarter_back))) %>%
    dplyr::mutate(pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2])) %>%
    dplyr::mutate(prey_size=substr(prey_size,cut_prey_size[1],cut_prey_size[2])) %>%
    dplyr::mutate(pp=paste(prey_name,prey_size)) %>% dplyr::mutate(prey_w1=prey_w) %>%
    dplyr::select(year,quarter,pred_name, pred_size,pp,prey_w1)

  control<-get_control(d2)
  x2<-as.data.frame(d2) %>%
    dplyr::mutate(year=as.integer(eval(control@strata_year_back)),quarter=as.integer(eval(control@strata_quarter_back))) %>%
    dplyr::mutate(pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2])) %>%
    dplyr::mutate(prey_size=substr(prey_size,cut_prey_size[1],cut_prey_size[2])) %>%
    dplyr::mutate(pp=paste(prey_name,prey_size)) %>% dplyr::mutate(prey_w2=prey_w) %>%
    dplyr::select(year,quarter,pred_name, pred_size,pp,prey_w2)

  x<-dplyr::full_join(x1,x2, by = c("year","quarter", "pred_name", "pred_size", "pp")) %>%
    dplyr::mutate(stratum_time=paste(year,quarter,sep='-Q'),dif_w=prey_w1-prey_w2)

  if (relative) x<-mutate(x,dif_w2=prey_w1/prey_w2) else x<-mutate(x,dif_w2=dif_w)
  x<-mutate(x,dif_w2=if_else(dif_w2>maxDif,maxDif,dif_w2))

  if (byVar=='year-quarter') {x$key<-paste(x$pred_name,x$year,x$quarter)}
  if (byVar=='year')         {x$key<-paste(x$pred_name,x$year)}
  if (byVar=='quarter')      {x$key<-paste(x$pred_name,x$quarter,x$pred_name)}
  if (byVar=='none')         {x$key<-paste(x$pred_name) }

  x<-  dplyr::mutate(x,size_w=if_else(is.na(dif_w),sqrt(size_NA),sqrt(abs(dif_w2))),
                     col=if_else(is.na(dif_w),'mis',if_else(dif_w>0,'pos','neg'))) %>%
    dplyr::mutate(col=factor(col,levels=c('pos','neg','mis'))) %>%
    dplyr::select(key,stratum_time,pred_name, pred_size,pp,size_w,col,prey_w1,prey_w2)

  out<-by(x,list(x$key),function(x) {
    if (addTitle) tit<- as.character(x$pred_name)[1] else tit<-NULL
    a<- ggplot(x, aes(pred_size, pp))+
      geom_point(aes(size = size_w, colour = col))+
      #scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
      scale_color_manual(values=Colours)+
      facet_grid( cols=vars(stratum_time),scales = "free_x", drop = TRUE)+
      labs(x='Predator size',y='Prey',title=tit)+
      theme(axis.text.x = element_text(angle = tAngle, vjust = 0.5 ),legend.position = 'none')
    if (show_plot) print(a) else return(a)
  })
  if (show_plot) return() else return(out)
}


#' Change predator or prey names.
#'
#' @param d Diet data set of class STOMdiet.
#' @param pred_from_to Change predator name (variable pred_name) from an external lookup table using first element of pred_from_to as key and second element of pred_from_to as new name.
#' @param prey_from_to Change prey name Same as above, but for prey name.
#' @param sp_info_file CSV file with the fields used for \code{pred_from_to}  and \code{prey_from_to} and and additional field "number" used for arranging predator and prey names as factors.
#' @param refactor Logical for re-factor of predator and prey names in accordance with the sequence given by the "number" variable in the \code{sp_info_file}
#' @return update data of class STOMdiet.
#' @export from_to_species_diet
from_to_species_diet<-function(d,pred_from_to=c("xcode","xshort"),prey_from_to=c("xSMS_species","xshort"),sp_info_file=file.path(config_dir,'species_info.csv'),refactor=TRUE){

  config_dir<-new_pred_name<-new_prey_name<-number<-pred_name<-prey_name<-NULL

  oth<-attr(d,'control')@other
  dd<-as.data.frame(d)
  a<- readr::read_csv(file= sp_info_file,col_types = readr::cols())

  if (pred_from_to[1] %in%  names(a) & pred_from_to[1] %in%  names(a)) {
    aa<- a %>% dplyr::mutate(pred_name=get(pred_from_to[1]),new_pred_name=get(pred_from_to[2])) %>%
      dplyr::select(pred_name,new_pred_name,number)
    dd<-dplyr::left_join(dd,aa,by='pred_name') %>% mutate(pred_name=NULL) %>%
      rename(pred_name=new_pred_name,pred_id_number=number)

    if (refactor) {
      aa <- aa %>% filter(new_pred_name %in% unique(dd$pred_name)) %>% unique() %>% arrange(number)
      aa <- bind_rows(filter(aa,number>0),filter(aa,number==0)) %>%
        mutate(number=1:n())
      dd$pred_name<-forcats::fct_relevel(dd$pred_name,aa$new_pred_name)
    }
  }

  levels(dd$pred_name)

  if (prey_from_to[1] %in% names(a) & prey_from_to[1] %in% names(a)) {
    aa<- a %>% dplyr::mutate(prey_name=get(prey_from_to[1]),new_prey_name=get(prey_from_to[2])) %>%
      dplyr::select(prey_name,new_prey_name,number)
    dd<-dplyr::left_join(dd,aa,by='prey_name') %>% mutate(prey_name=NULL) %>%
      rename(prey_name=new_prey_name,prey_id_number=number)

    if (refactor) {
      aa <- aa %>% filter(new_prey_name %in% unique(dd$prey_name)) %>% unique() %>% arrange(number)
      aa <- bind_rows(filter(aa,number>0),filter(aa,number==0)) %>%
        mutate(number=1:n())
      dd$prey_name<-forcats::fct_relevel(dd$prey_name,aa$new_prey_name)
    }


  }
  return(as_STOMdiet(dd))
}

