#' Allocate weight to individual prey lengths from prey-pooled weights using length-weight relations by prey species
#' and digestion stage from records where prey weights are recorded by the individual record.
#' This is done for the species included in the list of species (control@sel_species).
#' For other species, a total weight by species is calculated and length information is set to missing.
#' @title Allocate weight to individual prey lengths from prey-pooled weights
#' @param stom input Stomach contents data of class STOMobs.
#' @param sum_other_preys  Flag for transforming preys not included in sel_preys into prey="other food" with no length information
#' @param do_plots  Flag for plotting derived length-weight relations and other plots.
#' @param quantile_lw Quantile of size range of prey length to be used for estimating length-weight relation
#' @importFrom stats as.formula coef ftable lm quantile xtabs
#' @return Stomach data, class STOMobs.
#' @export
#' @examples \dontrun{individual_prey_weights_from_pooled_weight(stom=a,sel_preys=c("Clupea harengus"),
#'       sum_other_preys=TRUE,do_plots=FALSE)}
prey_w_from_pooled_weight<-function(stom,sum_other_preys=TRUE,do_plots=FALSE,quantile_lw=c(0.01,0.99)) {

  a<-b<-calc_prey_w<-digest<-fish_id<-lw_a<-n<-pool_prey_w<-prey_l<-prey_n<-prey_name<-prey_w<-prey_w_meth<-records<-sample_id<-sum_calc_prey_w<-sum_mis_l<-weight_pool_id<-weight_sample_id<-NULL

  if (FALSE) {  # for debugging
    sum_other_preys<-TRUE
    do_plots<-FALSE
    do_debug<-TRUE  # set do_debug=TRUE for debugging only
    quantile_lw<-c(0.01,0.99)
    stom<-a
    my.fish_id<-c("38126",  "97787","45964")
   } else do_debug<-FALSE


   control<-attr(stom,'control')
   sel_preys<-control@sel_preys
   mis_l <- as.integer(control@mis_l)
   other<-control@other  # id for "other food"
   mis_digest<-control@mis_digest

   stom[['PREY']]$prey_l<-as.integer(stom[['PREY']]$prey_l)

   if (do_debug) {
    sum(stom[['PREY']]$prey_w,na.rm=TRUE)
    data.frame(dplyr::filter(stom[['PREY']],fish_id %in% my.fish_id) %>% select(-sample_id))
  }

  # add variable for each weighting group (assuming that a recorded weight per prey name represent a dplyr::distinct group  )
  prey<-stom[['PREY']] %>% dplyr::mutate(weight_sample_id=paste(sample_id,fish_id,sep='-'),weight_pool_id=paste(weight_sample_id,prey_name,prey_w,sep='-'),pool_prey_w=prey_w)
  if (do_debug) {
    sum(prey$prey_w,na.rm=TRUE)
    dplyr::filter(prey,fish_id %in% my.fish_id) %>% select(-sample_id)
  }

  # records to be changed
  lat0<-dplyr::filter(prey,prey_w_meth=='p' & prey_name %in% sel_preys)
  if (do_debug) {
    dplyr::filter(lat0,fish_id %in% my.fish_id)%>% select(-sample_id,-weight_sample_id)
    xtabs(~prey_name+digest,data=dplyr::filter(lat0,prey_name %in% sel_preys),addNA = TRUE)
  }

  l1<- lat0 %>% dplyr::select(weight_sample_id,weight_pool_id) %>% dplyr::group_by(weight_sample_id,weight_pool_id) %>% dplyr::summarise(records=dplyr::n())
  l<-dplyr::left_join(lat0,l1, by = c("weight_sample_id", "weight_pool_id"))
  if (do_debug)   dplyr::filter(l,fish_id %in% my.fish_id)%>% select(-sample_id,-weight_sample_id)

  if (do_debug) {
    l_deb <- l %>% dplyr::select(fish_id,weight_sample_id,weight_pool_id,prey_w_meth,prey_name,prey_l,digest,pool_prey_w,prey_n,records)
    dplyr::filter(l_deb,fish_id %in% my.fish_id) %>% select(-weight_sample_id)
    dplyr::filter(l_deb,records>1)
  }

  # extract data for length weight relation of preys
  lw<-dplyr::filter(prey, prey_n==1 & prey_l!=mis_l & prey_w_meth=='r' & prey_name %in% sel_preys )

  # data for length weight relation
  lw<-dplyr::filter(lw, prey_l>=50 &  prey_l<=300 & prey_l !=mis_l) %>% droplevels()
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

  # estimate parameter a and b in W=a*l^b
  lw_par<-by(lw,list(lw$prey_name,lw$digest), function(x) {
    x<- x %>% dplyr::mutate(qlow=quantile(prey_l,probs=quantile_lw[1]),qhigh=quantile(prey_l,probs=quantile_lw[2])) %>% dplyr::filter(prey_l>=qlow & prey_l <=qhigh)
    b<-lm(log(prey_w) ~log(prey_l),data=x)
    p<-coef(b)
    data.frame(prey_name=x[1,'prey_name'],digest=x[1,'digest'],a=p[1],b=p[2])
  })

  ab<-dplyr::as_tibble(do.call(rbind,lw_par))
  ab


  # calculate weight from length and l-w relation
  l2<-dplyr::left_join(l,ab,by = c("prey_name", "digest")) %>% dplyr::mutate(calc_prey_w=exp(a+log(prey_l)* b) * prey_n,a=NULL,b=NULL)
  l2[l2$prey_l==mis_l,"calc_prey_w"]<-NA


  if (do_debug) {
    print(n=30,dplyr::filter(stom[['PREY']],fish_id %in% my.fish_id)%>% dplyr::select(-sample_id))
    print(n=30,dplyr::filter(l2,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id ))
   }

  # sum prey weights by weight pool
  l2<-l2 %>% dplyr::group_by( weight_sample_id,weight_pool_id) %>% dplyr::mutate(sum_calc_prey_w=sum(calc_prey_w,na.rm=TRUE),mis_len=prey_l==mis_l)

  if (do_debug) dplyr::filter(l2,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id )


  # number of records with missing length within weight_pool_id

  # number of records with missing length within weight_pool_id
  l2<-l2 %>%  dplyr::group_by( weight_sample_id,weight_pool_id) %>% dplyr::mutate(sum_mis_l=sum(mis_len,na.rm=TRUE)) %>% dplyr::mutate(mis_len=NULL) %>% ungroup()
  if (do_debug) {
    print(n=30,dplyr::filter(l2,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id ))
  }


  if (do_debug) head(dplyr::filter(l2,records>1),20)
  l_multi<-dplyr::filter(l2,records>1)
  l_multi0<-dplyr::filter(l_multi,sum_mis_l==0) %>% dplyr::mutate(prey_w=calc_prey_w*pool_prey_w / sum_calc_prey_w)
  if (do_debug) dplyr::filter(l_multi0,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id )

  # record with missing prey_w
  l_multi00<-dplyr::filter(l_multi0,is.na(prey_w))
  l_multi00<-l_multi00 %>%dplyr::select(-prey_l) %>% dplyr::distinct() %>% dplyr::mutate(prey_w=pool_prey_w,prey_l=NA)
  if (do_debug) dplyr::filter(l_multi00,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id )


  l_multi0<-dplyr::filter(l_multi0,!is.na(prey_w))
  if (do_debug) head(l_multi0,10)
  if (do_debug) head(data.frame(l_multi0),11)
  l_multi0<-dplyr::bind_rows(l_multi0,l_multi00)


  l_multi1<-dplyr::filter(l_multi,sum_mis_l>0)

  if (do_debug) {
    print(n=30,dplyr::filter(l_multi1,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id ))
  }

  l_multi11<- l_multi1 %>% dplyr::mutate(prey_w=dplyr::if_else(prey_l==mis_l,(pool_prey_w-sum_calc_prey_w)/ sum_mis_l,
                                                   dplyr::if_else(sum_calc_prey_w < pool_prey_w, calc_prey_w, calc_prey_w*pool_prey_w / sum_calc_prey_w))) %>%
                         dplyr::mutate(prey_w=dplyr::if_else(prey_l==mis_l & prey_w<0,0,prey_w)) #%>% dplyr::mutate(pool_prey_w=NA)

  if (do_debug) {
    print(n=30,dplyr::filter(l_multi11,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id ))
  }


  dplyr::filter(l_multi11,fish_id=="76712")
  del2<-          dplyr::filter(l_multi11,is.na(prey_w)) %>% dplyr::select(weight_sample_id) %>% dplyr::distinct()
  del2_sample_id<-dplyr::filter(l_multi11,is.na(prey_w)) %>% dplyr::select(sample_id,fish_id) %>% dplyr::distinct()

  # just cheking
  if (do_debug) {
    c1<- l_multi11 %>% dplyr::group_by ( weight_sample_id,weight_pool_id ) %>% dplyr::summarise(check=sum(prey_w,na.rm=TRUE))
    c2<- l_multi11 %>% dplyr::select( weight_sample_id,weight_pool_id,pool_prey_w ) %>%dplyr::distinct()
    dim(c1);dim(c2)
    dplyr::filter(dplyr::left_join(c1,c2),abs(check-pool_prey_w) >0.001)  # should be no records
  }  # end checking


  #### put it all together

  aa<-dplyr::bind_rows(l_multi0,l_multi11)

  if (do_debug)  print(n=30, dplyr::filter(aa,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id ))

  if (do_debug) {
    dplyr::filter(aa,fish_id=="76712") %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id )
    dplyr::filter(stom[['PREY']],fish_id=="76712")
  }

  aa<- aa%>% mutate(prey_w=if_else(is.na(prey_w),(pool_prey_w-sum_calc_prey_w)/sum_mis_l,prey_w))

  if (do_debug) {
    print(n=30,dplyr::filter(aa,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id,-weight_pool_id ))
    dplyr::filter(stom[['PREY']],fish_id=="76712")
  }

  aa<- aa%>%  dplyr::select(-records,-calc_prey_w,-sum_calc_prey_w,-sum_mis_l) %>%
      dplyr::mutate(weight_pool_id=NULL,pool_prey_w=NULL,prey_w_meth=NULL) %>% dplyr::filter(!is.na(prey_w))

  if (do_debug) {
     sort(unique(aa$prey_name))
     print(n=30,dplyr::filter(aa,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id ))
  }

  aa<- aa %>% dplyr::mutate(prey_ll=prey_l,prey_lu=prey_l,prey_w_meth=factor('r',levels=levels(prey$prey_w_meth)))

  if (do_debug) {
   print(n=30,dplyr::filter(stom[['PREY']],fish_id %in% my.fish_id))
   print(n=30,dplyr::filter(aa,fish_id %in% my.fish_id) %>%dplyr::select(-weight_sample_id))
  }


  oth<-dplyr::filter(prey,(prey_w_meth=='p' & !(prey_name %in% sel_preys)))

  if (do_debug) {
    dplyr::filter(oth,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id )
    dim(prey);dim(oth);dim(aa)
    print(setdiff(names(aa),names(oth)))
    print(setdiff(names(oth),names(aa)))
  }

  if (sum_other_preys) {
    oth<-oth %>% dplyr::group_by(weight_sample_id,weight_pool_id) %>% dplyr::mutate(n=1:dplyr::n()) %>% dplyr::ungroup()
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

  if (do_debug) {
    dplyr::filter(stom[['PREY']],fish_id %in% my.fish_id)
    dplyr::filter(bb,fish_id %in% my.fish_id)

  }


  if (do_debug) {dim(aa);dim(bb)}
  stopifnot(dim(aa)[[2]]==dim(bb)[[2]])

  remains<-dplyr::filter(prey,prey_w_meth!='p') %>% dplyr::mutate(weight_pool_id=NULL,pool_prey_w=NULL,prey_name=as.character(prey_name))
  if (do_debug) print(n=30,dplyr::filter(remains,fish_id %in% my.fish_id))
  if (do_debug) {dim(bb);dim(remains)}
  setdiff(names(bb),names(remains))
  setdiff(names(remains),names(bb))
  stopifnot(dim(bb)[[2]]==dim(remains)[[2]])

  bb<-dplyr::bind_rows(bb,remains)


  if (do_debug) {
    print(n=30,dplyr::filter(bb,fish_id %in% my.fish_id) %>% dplyr::select(-sample_id,-weight_sample_id ))
    print(n=30,dplyr::filter(stom[['PREY']],fish_id %in% my.fish_id) %>% dplyr::select(-sample_id))
  }
  if (do_debug) dim(bb)


  bb<-dplyr::anti_join(bb,del2_sample_id,by = c("sample_id", "fish_id"))

  if (do_debug) dim(bb)

  if (do_debug) summary(bb$prey_l)
  mis_ll_rec<-is.na(bb$prey_l)
  bb[mis_ll_rec,'prey_l']<-mis_l
  bb[mis_ll_rec,'prey_ll']<-mis_l
  bb[mis_ll_rec,'prey_lu']<-mis_l

  bb <- bb%>% dplyr::mutate(weight_sample_id=NULL,pool_prey_w=NULL,prey_name=fct_explicit_na(prey_name))

  # remove records from stom[['PRED']] where preys have been deleted due to errors
  stom[['PRED']]<-dplyr::anti_join(stom[['PRED']],del2_sample_id,by = c("sample_id", "fish_id"))

  stom[['PRED']]<-droplevels(stom[['PRED']])

  # re-factor$sample_id
  bb<-bb %>% dplyr::mutate(sample_id=factor(sample_id,levels=levels(stom[['PRED']]$sample_id)),
                           fish_id=factor(fish_id,levels=levels(stom[['PRED']]$fish_id)),prey_ll=as.integer(prey_ll),prey_lu=as.integer(prey_lu))


  if (do_debug) {

    print(n=30,dplyr::filter(bb,fish_id %in% my.fish_id) %>% dplyr::group_by(sample_id,fish_id, prey_name,digest) %>% dplyr::mutate(sumW=sum(prey_w,na.rm=TRUE)) %>% dplyr::select(-sample_id))
    print(n=30,dplyr::filter(stom[['PREY']],fish_id %in% my.fish_id) %>% dplyr::select(-sample_id))
  }

  stom[['PREY']]<-bb
  attr(stom,all_stom_attributes()["prey_w_id"])<-TRUE
  return(stom)
}
