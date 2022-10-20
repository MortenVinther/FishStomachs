#' Make bootstrap replicates of STOMobs object from stratification specified by control object, see (\link{STOMcontrol-class}  ).
#'
#' @title Make bootstrap replicates.
#' @param s Stomach data set of class STOMobs.
#' @param nfac Number of bootstrap samples relative to original number of samples.
#' @param replace Logical for bootstrapping with replacement.
#' @param seed Random seed value.
#' @param rep_id Replicate identifier
#' @return Bootstrapped stomach data (replicate) of class STOMobs.
#' @export
#' @examples \dontrun{b<-bootstrap_data(s)}
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
     dplyr::mutate(i=1:dplyr::n()) %>% dplyr::ungroup()
   x<-dplyr::inner_join(x,samp,by = "boots_id") %>%
      dplyr::mutate(sample_id=factor(paste(sample_id,i)),fish_id=factor(paste(fish_id,i)),rep_id=rep_id,n_boot=n)
 })

  x<-do.call(rbind,a);
  as_STOMobs(x,new_pred_var=c('rep_id','n_boot'))
}


#' Add variables for bootstrapping stratification.
#'
#' @title Add bootstrap stratification variables.
#' @param s Stomach data set of class STOMobs.
#' @return Stomach data set of class STOMobs with added variable boots_strata and boots_id.
#' @export
#' @examples \dontrun{bootstrap_addVar(stom)}
bootstrap_addVar<-function(s) {
  control<-get_control(s)
  s[['PRED']]<- s[['PRED']] %>%
    dplyr::mutate(boots_id=eval(control@bootstrapping$boots_id),
                  boots_strata=eval(control@bootstrapping$boots_strata))
  update.STOMobs(s)
}


#' Show stratification specified by control object, see (   ).
#'
#' @title Show bootstrap stratification.
#' @param s Stomach data set of class STOMobs.
#' @param show Show either number of stomachs per strata or number of stomachs per samples available for bootstrapping.
#' @param vari Variable, number of stomachs or number of samples
#' @return table Information on data available for bootstrapping
#' @export
#' @examples \dontrun{bootstrap_show(s)}
bootstrap_show<-function(s,show=c("strata",'sample')[1],vari=c("stomach","sample")[1]) {
  if (show=="strata" & vari=="stomach") return(xtabs(n_tot ~boots_strata+pred_size+pred_name,data=s[['PRED']])) else
  if (show=="sample" & vari=="stomach") return(xtabs(n_tot ~boots_id+pred_size+pred_name,data=s[['PRED']])) else
  if (show=="strata" & vari=="sample")  return(xtabs(      ~boots_strata+pred_size+pred_name,data=s[['PRED']])) else
  if (show=="sample" & vari=="sample")  return(xtabs(      ~boots_id+pred_size+pred_name,data=s[['PRED']]))
}



#' Add missing prey and prey size combination for bootstrap replicates
#'
#' @param bt Bootstrapped diet data (list).
#' @param mis_value Value used for missing prey weights in replicates. Uses control@model_options$minimum_stom as default
#' @return Bootstrapped diet data with inserted value for missing prey and prey size combinations
#' @export
#' @examples \dontrun{s<-add_missing(boots(s))}
add_missing_boots<-function(bt,mis_value) {

  pred_name<-pred_size_class<-prey_name<-prey_size<-prey_size_class<-repli<-stratum_area<-stratum_time<-NULL
  control<-attr(bt[[1]],'control')
  if (missing(mis_value)) mis_value<-control@model_options$minimum_stom

  #find all combinations of prey and prey sizes
  all<-lapply(bt,function(x){
    x<-as.data.frame(x)  %>% dplyr::select(stratum_area,stratum_time,pred_name,pred_size_class,prey_name,prey_size,prey_size_class)
    return(x)
  })
  all<-do.call(rbind,all) %>% unique()

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
      geom_histogram(col=Colours,bins=bins)+
      labs(x='Prey weight proportions (%)',y='Freqency',title=tit)+
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
#' @param freq Logical, show frequency (or density).
#' @param addTitle Add predator name on top of the plot.
#' @param tAngle Angle X-axis text.
#' @param Colours Colours for frequency.
#' @param maxbins maximum number of bins in plot.
#' @param statistics Data set (estimated by call to bootsMean) including statistics to be shown on the plot.
#' @return nothing (if show_plot=TRUE) or a list of plots.
#' @importFrom ggplot2 ggplot facet_grid geom_histogram geom_line geom_vline labs geom_text theme_minimal after_stat scale_fill_manual aes element_text element_line
#' @importFrom rlang .data
#' @export
plotboots<-function(b,show_plot=TRUE,freq=TRUE,cut_pred_size=c(1,10),addTitle=FALSE,tAngle=90,
                    Colours=c("red","green","blue"),maxbins=75,statistics) {
  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-quarter<-year<-NULL
  allNames<-prey_name<-prey_size<-stratum_time<-NULL

  control<-attr(b[[1]],'control')
  addStat<-FALSE
  if (!missing(statistics)) {
    stat<-statistics %>% select(year, quarter,pred_name,pred_size,prey,prey_name,n_prey_sp,phi,mu,param,mean_w) %>%
      mutate(pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2]))
    addStat<-TRUE
  }

  if (addStat) freq<-FALSE

  # to data frame
  x<-do.call(rbind,lapply(b,as.data.frame))
  reps<-max(x$rep_id)
  if (reps<maxbins) bins=reps else bins<-maxbins
  # calculate year and quarter from specifications, and sum up by prey
  x<-x %>%
    dplyr::mutate(year=as.integer(eval(control@strata_year_back)),
                  quarter=as.integer(eval(control@strata_quarter_back)),
                  pred_size=substr(pred_size,cut_pred_size[1],cut_pred_size[2])) %>%
    dplyr::group_by(stratum_time,year,quarter,pred_name, pred_size,prey_name,rep_id) %>%
    dplyr::summarise(prey_w=sum(prey_w)) %>% dplyr::ungroup()
  if (addStat) {
    x<-left_join(x,stat,by = c("year", "quarter", "pred_name", "pred_size", "prey_name"))
  }
  out<-by(x,list(x$pred_name,x$stratum_time),function(x) {
    binwidth_master<-range(x$prey_w,na.rm=TRUE)
    binwidth_master<-(binwidth_master[2]-binwidth_master[1])/100/bins
    if (addStat) {
      if (is.na(sum(x$phi))) addDiri<-FALSE else addDiri<-TRUE
      if (addDiri) {
         df<- x %>% group_by(pred_size,prey_name,mu,phi,param) %>%
           summarize(minPw=min(prey_w),maxPw=max(prey_w),n_rep_id=n()) %>%
           mutate( fromD=max(0.1,minPw*0.9), toD=min(99.9,maxPw*1.05), binwidth=(toD-fromD)/100/bins) %>% ungroup() %>%
           mutate(nbin=bins)
         df2<- df %>% group_by(pred_size,prey_name,mu,phi,param,binwidth,fromD,toD) %>%
           summarize(prey_w=seq(from=fromD,to=toD,length.out=bins),
                  y=dbeta(prey_w/100, shape1 = param, shape2 = phi-param))  %>% ungroup()

      } else {
        df<- select(x,pred_size,prey_name,mean_w) %>% unique()
      }
    }

    if (addTitle) tit<- paste(as.character(x$stratum_time)[1] ,as.character(x$pred_name)[1]) else tit<-NULL

    a<-ggplot(x,aes(prey_w))
    if (!freq) a<-a+geom_histogram(aes(y=after_stat(density)),  col=Colours[1],fill=Colours[2],bins=bins)
    if (freq)  a<-a+geom_histogram(aes(y=after_stat(count)),col=Colours[1],fill=Colours[2],bins=bins)

    a<-a+ labs(x='Prey weight proportions (%)',y=ifelse(freq,'Freqency','Density'),title=tit)+
      theme(axis.text.x = element_text(angle = tAngle, vjust = 0.5 ),legend.position = 'none')

    if (addStat)  {
      if (addDiri) {
        a<- a+geom_line(data = df2, aes(y=y*binwidth_master), color = "#C10534", size = 1, alpha = 0.75)+
           geom_vline(aes(xintercept=mu*100), linetype="dotted",color = Colours[3], size=1.5,data = df)
      } else {
        a<-a+geom_vline(aes(xintercept=mean_w*100), linetype="dotted",color = Colours[3], size=1.5,data = df)
      }
    }
    a<- a+facet_grid(vars(prey_name), vars(pred_size),scales = "free", drop = TRUE)

    if (show_plot) suppressWarnings(print(a)) else return(a)
  })
  if (show_plot) return() else return(out)
}


#' Statistics from bootstrap replicates.
#'
#' @param b Bootstrap diet data set. List of elements of class STOMdiet.
#' @param pointEst Diet data estimated without bootstrapping (point estimates of prey weights)
#' @param by_prey_size Logical for calculating mean and variance by prey size. FALSE provides statistics by prey species.
#' @param n_rep Selected replicates used. If n_rep is a number, 1:n_rep are used, If n_rep is a vector the provided replica numbers are used in the statistics. All replicates are uesed if n_rep is NA (default).
#' @param do_Diri Logical for performing statistics assuming the replicates are Dirichlet distributed.
#' @param minPreyProportion Lower level for average prey proportion to be kept as a named prey. If lower, the prey weight is allocated to "other" prey.
#' @param Diri_min Value to be used in estimating the Dirichelt parameter in case of a zero proportion.
#' @return Data set with mean and variance of diets and fit to Dirichlet distribution. Output includes
#' \tabular{ll}{
#' \strong{Variable} \tab \strong{Contents} \cr
#'  nboots    \tab Number of bootstrap replicates.\cr
#'  year      \tab Year.\cr
#'  quarter   \tab Quarter of the year.\cr
#'  pred_name \tab Predator name.\cr
#'  pred_size \tab Predator size.\cr
#'  prey      \tab Prey, prey name and size, if used.\cr
#'  prey_name \tab Prey name.\cr
#'  prey_size \tab Prey size.\cr
#'  n_prey_sp \tab Number of "preys" observed.\cr
#'  param     \tab Estimated parameters assuming a Dirichlet distribution of bootstrap replicates.\cr
#'  phi       \tab Estimated precision parameter (sum of param from the Dirichlet distribution).\cr
#'  mu        \tab Estimated mean value from the Dirichlet distribution.\cr
#'  p_value   \tab p-value for Log-likelihood ratio test for the estimated Dirichlet \cr \tab mean vector describe the bootstrap data well.\cr
#'  mean_w    \tab Mean value of prey weights (proportions).\cr
#'  sd_w      \tab Standard deviation of prey weights (proportions).\cr
#'  prey_w    \tab Point estimate of prey weight (proportions). \cr
#'  strata    \tab Strata used for bootstrap.\cr
#'  n_stom    \tab Number of stomachs available before bootstrap ( from the \code{pointEst} data).\cr
#'  n_sample  \tab Number of samples available before bootstrap.\cr
#' }
#' @importFrom Compositional diri.est dirimean.test
#' @export
bootsMean<-function(b,pointEst,by_prey_size=FALSE,n_rep=NA,do_Diri=FALSE,minPreyProportion=0.0,Diri_min=0.001) {
  key<-n_tot<-one<-pred_name<-pred_size<-prey_w<-quarter<-year<-NULL
  allNames<-prey_name<-prey_size<-stratum_time<-NULL

  control<-attr(b[[1]],'control')
  other<-control@other

  # to data frame
  x<-do.call(rbind,lapply(b,as.data.frame))
  if (! is.na(n_rep))  {if (length(n_rep)==1) x<-subset(x,rep_id<=n_rep) else  x<-subset(x,rep_id %in% n_rep)}
  if (by_prey_size) {
    x$prey_name<-as.factor(paste( x$prey_name,x$prey_size,sep=':::') )
    other<-paste(other,paste(control@mis_l,control@mis_l,sep='-'),sep=':::')
  }
  # calculate year and quarter from specifications, and sum up by prey if required
  x<-x %>%
    dplyr::mutate(year=as.integer(eval(control@strata_year_back)),
                  quarter=as.integer(eval(control@strata_quarter_back))) %>%
    dplyr::group_by(year,quarter,pred_name, pred_size,prey_name,rep_id) %>%
    dplyr::summarise(prey_w=sum(prey_w),.groups="keep") %>%
    dplyr::ungroup() %>% filter(!is.na(prey_w))

  #rescale to 1.00
  x<-x %>% group_by(year,quarter,pred_name, pred_size,rep_id) %>%
    dplyr::mutate(prey_w=prey_w/sum(prey_w))

  meanw<-x %>% dplyr::group_by(year,quarter,pred_name, pred_size,prey_name) %>% dplyr::summarise(incl=mean(prey_w)>=minPreyProportion)
  x<-dplyr::left_join(x,meanw,by = c("year", "quarter", "pred_name", "pred_size", "prey_name")) %>% dplyr::ungroup() %>%
    mutate(prey_name=if_else(incl,prey_name,factor(other,levels=levels(x$prey_name)))) %>%
    dplyr::group_by(year,quarter,pred_name, pred_size,prey_name,rep_id) %>% dplyr::summarise(prey_w=sum(prey_w))

  #rescale to 1.00 again (should no be necessary ?)
  x<-x %>% group_by(year,quarter,pred_name, pred_size,rep_id) %>%
    dplyr::mutate(prey_w=prey_w/sum(prey_w))

  out<-by(x,list(x$pred_name,x$pred_size,x$year,x$quarter),function(x) {

    x<-droplevels(x)
    preys<-levels(x$prey_name)
    xm<- x %>% dplyr::group_by(year,quarter,pred_name, pred_size,prey_name) %>%
      dplyr::summarise(mean_w=mean(prey_w),sd_w=sd(prey_w),n=n(),.groups="keep") %>%  dplyr::ungroup()
    a<-list(ok=TRUE,empirical=xm,preys=preys)
    if (do_Diri) {
      xx<-tidyr::pivot_wider(x,values_from=prey_w,names_from=prey_name)  %>% dplyr::ungroup()
      xx<-dplyr::select(xx,all_of(preys))
      xx<-as.matrix(xx)
      if (any(xx==0.0)) {
        xx[xx==0]<-Diri_min
        xx<-xx/apply(xx,1,sum) #rescale to 1.0
      }
      aa<-try(Compositional::diri.est(xx,type = "prec"),silent=TRUE) #MLE of the parameters of a Dirichlet distribution.
      if (class(aa)=="try-error") a$ok<-FALSE else a<-append(a,aa)
      if (a$ok) aa<-try(Compositional::dirimean.test(xx,aa$param),silent=TRUE) #Log-likelihood ratio test for a Dirichlet mean vector.
      if (class(aa)=="try-error") a$ok<-FALSE else a<-append(a,list(p_value=as.numeric(aa$info['p-value'])))
    }
    return(a)
  })

  b<-lapply(out,function(x) {
    if (x$ok & do_Diri)  data.frame(nboots=x$empirical$n,
                 year=x$empirical$year,quarter=x$empirical$quarter,
                 pred_name=x$empirical$pred_name,pred_size=x$empirical$pred_size,
                 prey=x$preys,n_prey_sp=length(x$preys),phi=x$phi,param=x$param,mu=x$mu,p_value=x$p_value,
                 mean_w=x$empirical$mean_w,sd_w=x$empirical$sd_w) else
               data.frame(nboots=x$empirical$n,year=x$empirical$year,quarter=x$empirical$quarter,
               pred_name=x$empirical$pred_name,pred_size=x$empirical$pred_size,
               prey=x$preys,n_prey_sp=length(x$preys),phi=NA,param=NA,mu=NA, p_value=NA,
               mean_w=x$empirical$mean_w,sd_w=x$empirical$sd_w)
  }
  )
  out<-do.call(rbind,b)
  if (missing(pointEst)) stop('A data set wth a point estimate of diet must be be provided')
 # samp<-left_join(
#    bootstrap_show(source,show=c("strata",'sample')[1],vari=c("stomach","sample")[1]) %>%
#      as.data.frame() %>% filter(Freq>0) %>% rename(n_stom=Freq),
#   bootstrap_show(source,show=c("strata",'sample')[1],vari=c("stomach","sample")[2]) %>%
#      as.data.frame() %>% filter(Freq>0) %>% rename(n_sample=Freq),
#    by = c("boots_strata", "pred_size", "pred_name")
 #  )
  pc<-get_control(pointEst)
  p<-as.data.frame(pointEst)
  if (by_prey_size) p$prey<-paste(p$prey_name,p$prey_size,sep=':::') else p$prey<-p$prey_name

  p<-p %>% mutate(year=as.integer(eval(pc@strata_year_back)),
                     quarter=as.integer(eval(pc@strata_quarter_back))) %>%
    group_by(year,quarter,pred_name,pred_size,n_tot,n_sample,prey) %>% summarize(prey_w=sum(prey_w)) %>%
    group_by(year,quarter,pred_name,pred_size,n_tot,n_sample) %>% mutate(prey_w=prey_w/sum(prey_w)) %>%
    rename(n_stom=n_tot) %>% ungroup()


   out<-left_join(out,p, by = c("year", "quarter", "pred_name", "pred_size", "prey"))

  if (by_prey_size) {
    xx<-matrix(unlist(strsplit(out$prey,':::')),ncol=2,byrow=TRUE)
    out$prey_name<-xx[,1]
    out$prey_name<-factor(out$prey_name,levels=levels(pointEst[['PREY']]$prey_name))
    out$prey_size<-xx[,2]
    out$prey_size<-factor(out$prey_size,levels=levels(pointEst[['PREY']]$prey_size))
  } else  {
    out$prey_name<-factor(out$prey,levels=levels(pointEst[['PREY']]$prey_name))
    out$prey_size<-'All'
  }
 return(dplyr::as_tibble(out))
}

