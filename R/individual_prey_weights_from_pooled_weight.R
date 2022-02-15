#' Allocate weight to individual prey lengths from prey-pooled weights using length-weight relations by prey species
#' and digestion stage from records where prey weights are recoderde by the individual record.
#' This is done for the species included in the list of species (control@sel_species).
#' For other species, a total weight by species is calculated and length information is set to missing.
#' @title Allocate weight to individual prey lengths from prey-pooled weights
#' @param stom input Stomach contents data of class STOMobs.
#' @param sel_preys Vector of prey names for processing.
#' @param sum_other_preys  Flag for transforming preys not included in sel_preys into prey="other food" with no length information
#' @param do_plots  Flag for plotting derived length-weight relations and other plots.
#' @param quantile_lw Quantile of size range of prey length to be used for estimating length-weight relation
#' @importFrom stats as.formula coef ftable lm quantile xtabs
#' @return Stomach data, class STOMobs.
#' @examples \dontrun{individual_prey_weights_from_pooled_weight(stom=a,sel_preys=c("Clupea harengus"),
#'       sum_other_preys=TRUE,do_plots=FALSE)}
prey_w_from_pooled_weight<-function(stom,sel_preys=c("Clupea harengus"),sum_other_preys=TRUE,do_plots=FALSE,quantile_lw=c(0.01,0.99)) {

   a<-b<-calc_prey_w<-digest<-fish_id<-lw_a<-n<-pool_prey_w<-prey_l<-prey_n<-prey_name<-prey_w<-prey_w_meth<-records<-sample_id<-sum_calc_prey_w<-sum_mis_l<-weight_pool_id<-weight_sample_id<-NULL
  control<-attr(stom,'control')
  if (FALSE) {  # for debugging
    sum_other_preys<-TRUE
    do_plots<-FALSE
    do_debug<-TRUE  # set do_debug=TRUE for debugging only
    quantile_lw<-c(0.01,0.99)
    stom<-a
   } else do_debug<-FALSE


   control<-attr(stom,'control')
   sel_preys<-control@sel_preys
   mis_l <- as.integer(control@mis_l)
   other<-control@other  # id for "other food"
   mis_digest<-9L  # skulle komme fra control

  # add variable for each weighing group (assuming that a recorded weight per prey name represent a dplyr::distinct group  )
  prey<-stom[['PREY']] %>% dplyr::mutate(weight_sample_id=paste(sample_id,fish_id,sep='-'),weight_pool_id=paste(weight_sample_id,prey_name,prey_w,sep='-'),pool_prey_w=prey_w)

  # records to be changed
  lat0<-dplyr::filter(prey,prey_w_meth=='p' & prey_name %in% sel_preys)
  #dplyr::filter(lat0,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)

  if (do_debug) xtabs(~prey_name+digest,data=dplyr::filter(lat0,prey_name %in% sel_preys),addNA = TRUE)

  l1<- lat0 %>% dplyr::select(weight_sample_id,weight_pool_id) %>% dplyr::group_by(weight_sample_id,weight_pool_id) %>% dplyr::summarise(records=n())
  l<-dplyr::left_join(lat0,l1, by = c("weight_sample_id", "weight_pool_id"))
  # dplyr::filter(l,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)

  if (do_debug) {
    l_deb <- l %>% dplyr::select(weight_sample_id,weight_pool_id,prey_w_meth,prey_name,prey_l,digest,pool_prey_w,prey_n,records)
    dplyr::filter(l_deb,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")
    dplyr::filter(l_deb,records>1)
  }

  # extract data for length weight relation of preys
  lw<-dplyr::filter(prey, prey_n==1 & !is.na(prey_l) & prey_w_meth=='r' & prey_name %in% sel_preys )

  if (do_plots) {
    by(lw,list(lw$prey_name), function(x) {
      print(ggplot(x,aes(prey_l,prey_w)) +
              theme_bw() +
              geom_point() +
              facet_wrap(~ paste(prey_name,digest), scales="free_y") +
              labs(x="Prey Length", y="Prey Weight",title="")
      )
    })
  }

  if (do_plots) cleanup()


  # data for length weight relation
  lw<-dplyr::filter(lw, prey_l>=50 &  prey_l<=300)
  lw<-lw %>% dplyr::mutate(lw_a=prey_w/prey_l^3) %>% dplyr::filter(lw_a<1.5e-5  & lw_a>1e-6)

  #summary(lw)
  if (do_plots) {
    by(lw,list(lw$prey_name), function(x) {
      print(ggplot(x,aes(lw_a)) +
              theme_bw() +
              geom_histogram() +
              facet_wrap(~ paste(prey_name,digest), scales="free_y") +
              labs(x="a in w=a*lenght^3", y="frequency",title="")
      )
    })

  }

  if (do_debug) {
    cat("\nnumber of observations used for length weight relation\n")
    print(xtabs(~prey_name+digest,data=lw,addNA = TRUE))
  }

  if (do_plots) {
    by(lw,list(lw$prey_name), function(x) {

      print(ggplot(x,aes(prey_l,prey_w)) +
              theme_bw() +
              geom_point() +
              facet_wrap(~ paste(prey_name,digest), scales="free_y") +
              labs(x="Prey Length", y="Prey Weight",title="")
      )
    })
  }

  if (do_plots) cleanup()

  if (do_plots) {
    by(lw,list(lw$prey_name), function(x) {
      print(ggplot(x,aes(log(prey_l),log(prey_w))) +
              theme_bw() +
              geom_point() +
              facet_wrap(~ paste(prey_name,digest), scales="free_y") +
              labs(x="log Prey Length", y="log Prey Weight",title="")
      )
    })
  }
  if (do_plots) cleanup()

  # estimate parameter a and b in W=a*l^b
  lw_par<-by(lw,list(lw$prey_name,lw$digest), function(x) {
    x<- x %>% dplyr::mutate(qlow=quantile(prey_l,probs=quantile_lw[1]),qhigh=quantile(prey_l,probs=quantile_lw[2])) %>% dplyr::filter(prey_l>=qlow & prey_l <=qhigh)
    b<-lm(log(prey_w) ~log(prey_l),data=x)
    p<-coef(b)
    data.frame(prey_name=x[1,'prey_name'],digest=x[1,'digest'],a=p[1],b=p[2])
  })

  ab<-dplyr::as_tibble(do.call(rbind,lw_par))
  ab

  # there are observations with prey_l , but no prey_n!
  check<-dplyr::left_join(l,ab,by = c("prey_name", "digest"))
  check<-dplyr::filter(check,!is.na(prey_l) & !is.na(pool_prey_w) &is.na(prey_n)) %>% dplyr::mutate(calc_prey_w=exp(a+log(prey_l)* b))
  xtabs( ~digest,data=check)
  del0<-check %>% dplyr::select(weight_sample_id) %>% dplyr::distinct()
  del0

  l2<-dplyr::left_join(l,ab,by = c("prey_name", "digest")) %>% dplyr::mutate(calc_prey_w=exp(a+log(prey_l)* b) * prey_n,a=NULL,b=NULL)
  if (do_debug) {
    data.frame(l2)
    dplyr::filter(l2,digest==0 & records>1) #looks good
    dplyr::filter(l2,digest==1 & records>1)
    dplyr::filter(l2,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")
  }

  # sum prey weights by weight pool
  ll<-l2 %>% dplyr::group_by( weight_sample_id,weight_pool_id) %>% dplyr::summarise(sum_calc_prey_w=sum(calc_prey_w,na.rm=TRUE))
  if (do_debug)  dplyr::filter(ll,is.na(sum_calc_prey_w))

  l2<-dplyr::left_join(l2,ll,by = c("weight_sample_id", "weight_pool_id")) %>% dplyr::mutate(mis_l=is.na(prey_l))
  #dplyr::filter(l2,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")

  # number of records with missing length within weight_pool_id
  ll<-l2 %>%  dplyr::group_by( weight_sample_id,weight_pool_id) %>% dplyr::summarise(sum_mis_l=sum(mis_l,na.rm=TRUE)) %>% dplyr::mutate(mis_l=NULL)
  l2<-dplyr::left_join(l2,ll,by = c("weight_sample_id", "weight_pool_id")) %>% dplyr::mutate(mis_l=NULL)
  l2<-dplyr::ungroup(l2)
  if (do_debug) {
    l2
    dplyr::filter(l2,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")
  }


  if (do_debug) head(dplyr::filter(l2,records>1),20)
  l_multi<-dplyr::filter(l2,records>1)
  l_multi0<-dplyr::filter(l_multi,sum_mis_l==0) %>% dplyr::mutate(prey_w=calc_prey_w*pool_prey_w / sum_calc_prey_w)
  # dplyr::filter(l_multi0,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")

  # record with missing prey_w
  l_multi00<-dplyr::filter(l_multi0,is.na(prey_w))
  l_multi00<-l_multi00 %>%dplyr::select(-prey_l) %>% dplyr::distinct() %>% dplyr::mutate(prey_w=pool_prey_w,prey_l=NA)
  # dplyr::filter(l_multi00,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")


  l_multi0<-dplyr::filter(l_multi0,!is.na(prey_w))
  if (do_debug) head(l_multi0,10)
  if (do_debug) head(data.frame(l_multi0),11)
  l_multi0<-dplyr::bind_rows(l_multi0,l_multi00)

  # dplyr::filter(l_multi0,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")

  l_multi1<-dplyr::filter(l_multi,sum_mis_l>0)
  if (do_debug) dplyr::filter(l_multi1,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")

  l_multi11<- l_multi1 %>% dplyr::mutate(prey_w=dplyr::if_else(is.na(prey_l),(pool_prey_w-sum_calc_prey_w)/ sum_mis_l,
                                                 dplyr::if_else(sum_calc_prey_w < pool_prey_w, calc_prey_w, calc_prey_w*pool_prey_w / sum_calc_prey_w))) %>%
                         dplyr::mutate(prey_w=dplyr::if_else(is.na(prey_l) & prey_w<0,0,prey_w)) #%>% dplyr::mutate(pool_prey_w=NA)

  if (do_debug) {
    l_multi11
    summary(l_multi11)
    dplyr::filter(l_multi11,is.na(prey_w))
    if (do_debug) dplyr::filter(l_multi11,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")
  }

  del2<-dplyr::filter(l_multi11,is.na(prey_w)) %>% dplyr::select(weight_sample_id) %>% dplyr::distinct()
  del2_sample_id<-dplyr::filter(l_multi11,is.na(prey_w)) %>% dplyr::select(sample_id,fish_id) %>% dplyr::distinct()

  # just cheking
  if (do_debug) {
    c1<- l_multi11 %>% dplyr::group_by ( weight_sample_id,weight_pool_id ) %>% dplyr::summarise(check=sum(prey_w,na.rm=TRUE))
    c2<- l_multi11 %>% dplyr::select( weight_sample_id,weight_pool_id,pool_prey_w ) %>%dplyr::distinct()
    dim(c1);dim(c2)
    dplyr::filter(dplyr::left_join(c1,c2),abs(check-pool_prey_w) >0.001)  # should be no records
  }  # end checking


  #### put it all together
  # just cheking
  if (do_debug) {
    l_multi0[is.na(l_multi0$prey_l),] #mulig fejl ?
    l_multi0[is.na(l_multi0$prey_l) & l_multi0$sum_calc_prey_w>0 & l_multi0$prey_w>0,] #mulig fejl ?

    l_multi1[is.na(l_multi1$prey_l),]
  }

  aa<-dplyr::bind_rows(l_multi0,l_multi11) %>%
      dplyr::select(-records,-calc_prey_w,-sum_calc_prey_w,-sum_mis_l) %>%
      dplyr::mutate(weight_pool_id=NULL,pool_prey_w=NULL,prey_w_meth=NULL) %>% dplyr::filter(!is.na(prey_w))

  if (do_debug) {
     sort(unique(aa$prey_name))
     aa
     summary(aa)
     dplyr::filter(aa,weight_sample_id=="Cod-1980-1-24-LAT-ZBA-26-41517")
  }


  aa<- aa %>% dplyr::mutate(prey_ll=prey_l,prey_lu=prey_l,prey_w_meth=factor('r',levels=levels(prey$prey_w_meth)))


    # dplyr::filter(aa,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)


  oth<-dplyr::filter(prey,(prey_w_meth=='p' & !(prey_name %in% sel_preys)))

    if (do_debug) {
    dplyr::filter(oth,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)
    dim(prey);dim(oth);dim(aa)
    print(setdiff(names(aa),names(oth)))
    print(setdiff(names(oth),names(aa)))
  }

  if (sum_other_preys) {
    oth<-oth %>% dplyr::group_by(weight_sample_id,weight_pool_id) %>% dplyr::mutate(n=1:n()) %>% dplyr::ungroup()
    #delete replicates
    oth<-dplyr::filter(oth,n==1) %>%
      dplyr::mutate(prey_w=pool_prey_w,n=NULL)
    # put it into "other food"
    oth <- oth %>% dplyr::mutate(prey_name=other,prey_l=NA,prey_ll=mis_l,prey_lu=mis_l,digest=mis_digest,prey_w_meth=factor('r',levels=levels(prey$prey_w_meth)))
    #head(data.frame(oth))
  }

  oth<-oth %>% dplyr::mutate(weight_pool_id=NULL, pool_prey_w=NULL)
  if (do_debug) {
    head(data.frame(aa))
     print(setdiff(names(aa),names(oth)))
     print(setdiff(names(oth),names(aa)))
     dim(aa);dim(oth)
  }
  stopifnot(dim(aa)[[2]]==dim(oth)[[2]])
  aa<- aa %>% dplyr::mutate(prey_name=as.character(prey_name))
  bb<-dplyr::bind_rows(aa,oth)

  if (do_debug) {dim(aa);dim(bb)}
  stopifnot(dim(aa)[[2]]==dim(bb)[[2]])

  remains<-dplyr::filter(prey,prey_w_meth!='p') %>% dplyr::mutate(weight_pool_id=NULL,pool_prey_w=NULL,prey_name=as.character(prey_name))
  # dplyr::filter(remains,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)
  if (do_debug) {dim(bb);dim(remains)}
  setdiff(names(bb),names(remains))
  setdiff(names(remains),names(bb))
  stopifnot(dim(bb)[[2]]==dim(remains)[[2]])


  bb<-dplyr::bind_rows(bb,remains)
  # dplyr::filter(bb,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)
  if (do_debug) dim(bb)
  #bb<-dplyr::filter(bb,!(weight_sample_id %in% del2$weight_sample_id))
  bb<-dplyr::anti_join(bb,del2_sample_id,by = c("sample_id", "fish_id"))


  if (do_debug) dim(bb)

 #dplyr::filter(bb,year==1980 & quarter==3 & prey_name=='Sprattus sprattus')
 # dplyr::filter(bb,sample_id=="Cod-1980-1-24-LAT-ZBA-26" & fish_id==41517)
  mis_ll_rec<-is.na(bb$prey_l)
  bb[mis_ll_rec,'prey_l']<-mis_l
  bb[mis_ll_rec,'prey_ll']<-mis_l
  bb[mis_ll_rec,'prey_lu']<-mis_l

  bb <- bb%>% dplyr::mutate(weight_sample_id=NULL,pool_prey_w=NULL,prey_name=fct_explicit_na(prey_name))

  # remove records from stom[['PRED']] where preys have been deleted due to errors
  stom[['PRED']]<-dplyr::anti_join(stom[['PRED']],del2_sample_id,by = c("sample_id", "fish_id"))

  stom[['PRED']]<-droplevels(stom[['PRED']])

  # re-factor$sample_id
  bb<-bb %>% dplyr::mutate(sample_id=factor(sample_id,levels=levels(stom[['PRED']]$sample_id)),fish_id=factor(fish_id,levels=levels(stom[['PRED']]$fish_id)))

    stom[['PREY']]<-bb
  attr(stom,all_stom_attributes()["prey_w_id"])<-TRUE
  return(stom)
}
