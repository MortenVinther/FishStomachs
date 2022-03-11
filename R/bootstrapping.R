#' Extract bootstrap data from stratification specified by control object, see (   ).
#'
#' @title Check if data are ready for aggregation.
#' @param s Stomach data set of class STOMobs.
#' @param nfac Number of bootstrap samples relative to original number of samples.
#' @param replace Logical for bootstrapping with replacement.
#' @param seed Random seed value.
#' @param rep_id Replicate identifier
#' @return s Bootstrapped stomach data set of class STOMobs.
#' @export
#' @examples \dontrun{check_for_missing_info_before_strata_aggregation(stom)}
bootstrap_data<-function(s,nfac=1,replace=TRUE,seed=0,rep_id=1) {

 boots_id<-fish_id<-i<-sample_id<-NULL

 control<-get_control(s)
 set.seed(seed)
 s[['PRED']]<- s[['PRED']] %>%
   dplyr::mutate(boots_id=eval(control@bootstrapping$boots_id),
                 boots_strata=eval(control@bootstrapping$boots_strata))
 ss<-as.data.frame(s)

 #sample from 1 to ... and draw samples for each strata
  a<-by(ss,list(ss$boots_strata),function(x) {
   x<-dplyr::mutate(x,boots_id=as.integer(factor(boots_id)))
   n<-as.integer(max(x$boots_id))
   samples<-sample.int(n,size=as.integer(n*nfac),replace=replace)
   samp<-data.frame(boots_id=samples) %>% dplyr::group_by(boots_id) %>%
     dplyr::mutate(i=1:n()) %>% dplyr::ungroup()
   x<-dplyr::inner_join(x,samp,by = "boots_id") %>%
      dplyr::mutate(sample_id=factor(paste(sample_id,i)),fish_id=factor(paste(fish_id,i)),rep_id=rep_id)
 })

  x<-do.call(rbind,a);
  as_STOMobs(x,new_pred_var='rep_id')
}

#' Show stratification specified by control object, see (   ).
#'
#' @title Show bootstrap stratification.
#' @param s Stomach data set of class STOMobs.
#' @param show Show either number of stomachs per strata or number of stomachs per samples available for bootstrapping.
#' @return table Information on data available for bootstrapping
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



#' Add missing prey, prey size combination
#'
#' @param bt Bootstrapped diet data.
#' @param mis_value Value for missing prey weights.
#' @return Bootstrapped diet data with inserted value for missing prey, prey size combinations
#' @export
#' @examples \dontrun{check_for_missing_info_before_strata_aggregation(stom)}
add_missing_boots<-function(bt,mis_value=0) {

  pred_name<-pred_size_class<-prey_name<-prey_size<-prey_size_class<-repli<-stratum_area<-stratum_time<-NULL

  #find all combinations of prey and prey sizes
  all<-lapply(bt,function(x){
    x<-as.data.frame(x)  %>% dplyr::select(stratum_area,stratum_time,pred_name,pred_size_class,prey_name,prey_size,prey_size_class)
    return(x)
  })
  all<-do.call(rbind,all) %>% unique()
  subset(all,pred_size_class==4 & stratum_time=="1991-Q1" & prey_name=="Sandeel")


  # add all combinations of prey and prey sizes in any replicate
  bt2<-lapply(bt,function(x){

    a<-dplyr::anti_join(x=all,y=as.data.frame(x),
                by = c("stratum_area", "stratum_time", "pred_size_class", "pred_name","prey_name", "prey_size", "prey_size_class")) %>%
      dplyr::mutate(prey_w=mis_value)

    a<- dplyr::right_join(x[['PRED']],a, by = c("stratum_area", "stratum_time", "pred_size_class", "pred_name")) %>%
        dplyr::select(all_of(names(x[['PREY']])))

    x[['PREY']]<-rbind(x[['PREY']],a)
    return(x)
  })
  return(bt2)
}



#' Plot bootstrapping diet data.
#'
#' @param b Bootstrap diet data set of class STOMdiet.
#' @param cut_pred_size From to in substring of predator size
#' @param cut_prey_size From to in substring of prey size
#' @param show_plot Show the resulting graphs on screen (or save the results for later processing)
#' @param addTitle Add predator name on top of the plot.
#' @param tAngle Angle X-axis text.
#' @param Colours Colours for frequency.
#' @param maxbins maximum number of bins in plot.
#' @return nothing (if show_plot=TRUE) or a list of plots.
#' @importFrom ggplot2 ggplot facet_grid geom_histogram labs geom_text theme_minimal scale_fill_manual aes element_text element_line
#' @importFrom rlang .data
#' @export
plotboots.size<-function(b,show_plot=TRUE,cut_pred_size=c(1,10),cut_prey_size=c(1,10),addTitle=FALSE,tAngle=90,
                   Colours='red',maxbins=50) {
  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-quarter<-year<-NULL
  allNames<-prey_name<-prey_size<-stratum_time<-NULL

  control<-attr(b[[1]],'control')
  # to one data frame
  x<-do.call(rbind,lapply(b,as.data.frame))
  reps<-max(x$rep_id)
  if (reps<maxbins) bins=reps else bins<-maxbins

  # calculate year and quarter from specifications
  x<-x %>%
    dplyr::mutate(year=as.integer(eval(control@strata_year_back)),
                  quarter=as.integer(eval(control@strata_quarter_back)),
                  pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2]),
                  prey_size=substr(prey_size,cut_prey_size[1],cut_prey_size[2])) %>%
    dplyr::select(key,stratum_time,pred_name, pred_size,n_tot,prey_name,prey_size,prey_w,rep_id)


  out<-by(x,list(x$pred_name,x$pred_size,x$stratum_time),function(x) {
    if (addTitle) tit<- paste(as.character(x$stratum_time)[1] ,as.character(x$pred_name)[1],as.character(x$pred_size)[1],sep=', ') else tit<-NULL
    a<-ggplot(x,aes(prey_w)) +
      facet_grid(vars(prey_name), vars(prey_size),scales = "free", drop = TRUE)+
      scale_fill_manual(  values = allNames,name='Prey')+
      geom_histogram(col=Colours,bins=bins)+
      labs(x='Prey weight %',y='Freqency',title=tit)+
      theme(axis.text.x = element_text(angle = tAngle, vjust = 0.5 ),legend.position = 'none')
      if (show_plot) suppressWarnings(print(a)) else return(a)
  })
  if (show_plot) return() else return(out)
}



#' Plot bootstrapping diet data.
#'
#' @param b Bootstrap diet data set of class STOMdiet.
#' @param cut_pred_size From to in substring of predator size
#' @param show_plot Show the resulting graphs on screen (or save the results for later processing)
#' @param addTitle Add predator name on top of the plot.
#' @param tAngle Angle X-axis text.
#' @param Colours Colours for frequncy.
#' @param maxbins maximum number of bins in plot.
#' @return nothing (if show_plot=TRUE) or a list of plots.
#' @importFrom ggplot2 ggplot facet_grid geom_histogram labs geom_text theme_minimal scale_fill_manual aes element_text element_line
#' @importFrom rlang .data
#' @export
plotboots<-function(b,show_plot=TRUE,cut_pred_size=c(1,10),addTitle=FALSE,tAngle=90,
                         Colours='red',maxbins=50) {
  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-quarter<-year<-NULL
  allNames<-prey_name<-prey_size<-stratum_time<-NULL

  control<-attr(b[[1]],'control')
  # to one data frame
  x<-do.call(rbind,lapply(b,as.data.frame))
  reps<-max(x$rep_id)
  if (reps<maxbins) bins=reps else bins<-maxbins
  # calculate year and quarter from specifications, and sum up by prey
  x<-x %>%
    dplyr::mutate(year=as.integer(eval(control@strata_year_back)),
                  quarter=as.integer(eval(control@strata_quarter_back)),
                  pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2])) %>%
                  dplyr::group_by(stratum_time,pred_name, pred_size,n_tot,prey_name,rep_id) %>%
                       dplyr::summarise(prey_w=sum(prey_w)) %>%
                  dplyr::ungroup()

  out<-by(x,list(x$pred_name,x$stratum_time),function(x) {
    if (addTitle) tit<- paste(as.character(x$stratum_time)[1] ,as.character(x$pred_name)[1]) else tit<-NULL
    a<-ggplot(x,aes(prey_w)) +
      facet_grid(vars(prey_name), vars(pred_size),scales = "free", drop = TRUE)+
      scale_fill_manual(  values = allNames,name='Prey')+
      geom_histogram(col=Colours,bins=bins)+
     # geom_histogram(col=Colours,binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+

      labs(x='Prey weight %',y='Freqency',title=tit)+
      theme(axis.text.x = element_text(angle = tAngle, vjust = 0.5 ),legend.position = 'none')
    if (show_plot) suppressWarnings(print(a)) else return(a)
  })
  if (show_plot) return() else return(out)
}

