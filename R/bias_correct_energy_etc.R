#' Bias correct stomach content due to prey energy density and armament.
#' @title Bias correct stomach content due to energy contents, armament etc.
#' @param x Stomach data of class \code{STOMobs}
#' @param nodc_group_file a
#' @param param_file b
#' @param temperature_file c
#' @param energy_file d
#' @param hour_multiplier The stomach contents prey weight (prey_w) is transformed into an eaten food ration (grammes per hour) which can be considered a as proxy for stomach contents weight.food ration
#' The unit, grammes per hour can be changed by the hour_multiplier, e.g \code{hour_multiplier=2190} will provide g/quarter of the year.;
#' @param pred_length_multiplier Multiplier for predator length (to derive the predator length used (in cm) from the predator length variable (pred_l)used in data)
#' @return Stomach contents data of class \code{STOMobs} where the observed prey weights has been recalculated to ration eaten by hour (or any other unit chosen by \code{hour_multiplier})
#' @export
#'
bias_correct_energy_etc<-function(x,nodc_group_file,param_file,temperature_file,energy_file,hour_multiplier=1,pred_length_multiplier=1) {

  First<-Last<-pred_nodc<-pred_name<-sample_id<-fish_id<-year<-quarter<-rectangle<-pred_l<-pred_ll<-n_tot<-n_food<-n_empty_n_regur<-NULL
  Temp<-meanT<-delta<-rho<-lambda<-xi<-alfa<-k<-l<-Armer<-E<-prey_ll<-prey_w<-avg_E<-sum_w<-b<-S<-prey_n<-just_regur<-species_group<-NULL
  prey_nodc<-ration1<-evacRate<-NULL

  if (FALSE) { #test
    x<-a
    nodc_group_file<-file.path(system.file( package = "FishStomachs"),"extdata","consum_nodc.csv")
    param_file<-file.path(system.file( package = "FishStomachs"),"extdata","parameters.csv")
    temperature_file=file.path(system.file( package = "FishStomachs"),"extdata","temperature.csv")
    energy_file<-file.path(system.file( package = "FishStomachs"),"extdata","energy_density_and_armament.csv")
    hour_multiplier<-2156
  }

  control<-attr(x,'control')
  mis_l <- as.integer(control@mis_l)

  if (missing(nodc_group_file))  stop(paste('An input file must be given, nodc_group_file.\n See file', file.path(system.file( package = "FishStomachs"),"extdata","consum_nodc.csv"),'for format'))
  if (missing(param_file))       stop(paste('An input file must be given, param_file.\n See file', file.path(system.file( package = "FishStomachs"),"extdata","parameters.csv"),'for format'))
  if (missing(temperature_file)) stop(paste('An input file must be given, temperatur_file.\n See file', file.path(system.file( package = "FishStomachs"),"extdata","Temperature.csv"),'for format'))
  if (missing(energy_file))      stop(paste('An input file must be given, energy_file.\n See file', file.path(system.file( package = "FishStomachs"),"extdata","energy_density_and_armament.csv"),'for format'))

  cn<-readr::read_csv(nodc_group_file,col_types = readr::cols()) %>% dplyr::mutate(First=as.numeric(First),Last=as.numeric(Last))
  parm<-readr::read_csv(param_file,col_types = readr::cols()) %>% dplyr::select(-pred_nodc)
  temp<-readr::read_csv(temperature_file,col_types =readr::cols())
  energy<-readr::read_csv(energy_file,col_types = readr::cols()) %>% dplyr::distinct()

  x_unchanged<-subset(x,!(pred_name %in%  as.character(parm$pred_name)))
  x<-subset(x,pred_name %in%  as.character(parm$pred_name))


  # add parm
  parm<- dplyr::mutate (parm,pred_name=factor(pred_name,levels=levels(x[['PRED']]$pred_name))) %>% dplyr::filter(!is.na(pred_name))
  pred<-dplyr::left_join(dplyr::select(x[['PRED']],sample_id,fish_id,pred_name,year,quarter,rectangle,pred_l,pred_ll,n_tot,n_food),parm,by = "pred_name")

  # add temperature
  temp <-dplyr::mutate(temp,rectangle=factor(rectangle,levels=levels(x[['PRED']]$rectangle)),quarter=as.integer(quarter))
  pred<-dplyr::left_join(pred,temp,by = c("quarter", "rectangle"))

  if (any(is.na(pred$Temp)))  {
     avg_temp<-temp %>% dplyr::group_by(quarter) %>% dplyr::summarise(meanT=mean(Temp)) %>% dplyr::ungroup()
     pred<-dplyr::left_join(pred,avg_temp,by = "quarter") %>% dplyr::mutate(Temp=dplyr::if_else(is.na(Temp),meanT,Temp),meanT=NULL)
  }


  prey<-dplyr::left_join(x[['PREY']],dplyr::select(pred,pred_l,sample_id,fish_id,n_tot,n_food,quarter,delta,rho,lambda,xi,alfa,k,Temp),by = c("sample_id", "fish_id"))


  ## check
  nodc<-sort(unique(prey$prey_nodc))
  ff<-function(x) dplyr::filter(cn,x>=First & x<=Last) %>% dplyr::mutate(prey_nodc=x)

  aa<-lapply(nodc,ff)

  bb<-lapply(aa,dim)
  bb<-matrix(unlist(bb),ncol=2,byrow=TRUE)
  if (length(nodc[which(bb[,1]==0)]) >0) stop('missing NODC code for bias correction for energy, armament etc.')

  cn<-do.call(dplyr::bind_rows,aa) %>% dplyr::select(prey_nodc,species_group)
  prey<-dplyr::left_join(prey,cn,by="prey_nodc")


  en<-by(energy,list(energy$species_group,energy$quarter,energy$lower,energy$upper),function(x){
    data.frame(species_group=x$species_group,quarter=x$quarter,l=as.integer(seq(x$lower,x$upper,1)),E=x$E,Armer=x$Armer)
  })
  en<-dplyr::as_tibble(do.call(rbind,en))

  en2<-en %>% dplyr::filter(dplyr::between(l,50,200)) %>%
    dplyr::group_by(species_group,quarter) %>% dplyr::summarise(Armer=mean(Armer),E=mean(E)) %>%dplyr::mutate(l=mis_l)
  en<-dplyr::bind_rows(en,en2) %>% dplyr::mutate(species_group=as.character(species_group))

  prey<- dplyr::mutate(prey,l=dplyr::if_else(is.na(prey_ll),mis_l,prey_ll))

  prey<-dplyr::left_join(prey,en,by = c("quarter", "species_group", "l")) %>% dplyr::mutate(quarter=NULL,l=NULL)

  tst<-dplyr::filter(prey,is.na(E))  #something is wrong
  if (dim(tst)[[1]]>0) {
    cat("Energy and armament data were not found for the data shown below: \n")
    print(tst)
    cat(" Average values of Energy contents and Armament will be used'\n")
    av_arm<-mean(prey$Armer,na.rm=TRUE)
    av_E<-mean(prey$E,na.rm=TRUE)
    prey<-dplyr::mutate(prey,E=dplyr::if_else(is.na(E),av_E,E), Armer=dplyr::if_else(is.na(Armer),av_arm,Armer))
  }

  # prey<-dplyr::select(prey,-species_group)
  empty<-dplyr::filter(prey,n_food==0)
  prey<-dplyr::filter(prey,n_food>0)


  avg<-prey %>% dplyr::group_by(sample_id,fish_id) %>% dplyr::summarise(avg_E=sum(E*prey_w),sum_w=sum(prey_w)) %>%
      dplyr::mutate(avg_E=avg_E/sum_w)
  prey<-dplyr::left_join(prey,avg,by = c("sample_id", "fish_id"))


  prey<-prey %>% dplyr::mutate(b=prey_w/sum_w,S=sum_w/n_tot,sum_w=NULL,k=dplyr::if_else(n_tot==1L,1,k),pred_l=pred_l*pred_length_multiplier) %>%
          mutate(ration1=rho* b*Armer*(pred_l**lambda)*exp(delta*Temp)*(avg_E**xi)*k*((n_tot/n_food)**(alfa-1))*(S**alfa)*hour_multiplier,
          evacRate=rho*Armer*(pred_l**lambda)*exp(delta*Temp)*(avg_E**xi))

 # prey %>% arrange(sample_id,fish_id,prey_name) %>%dplyr::select( -pred_l, -n_tot, -n_food, -prey_ll, -prey_lu,- prey_n)
PREY<<-prey
  prey<-prey %>% dplyr::mutate(prey_w=ration1) %>%
      dplyr::select(-pred_l, -n_tot, -n_food, -delta, -rho,-lambda,-xi, -alfa, -k, -Temp,-E, -Armer, -avg_E,-S,-b,-species_group, -ration1, -evacRate)


  x[['PREY']]<-prey
  x<-c(x_unchanged,x)

  attr(x,all_stom_attributes()["bias_energy"])<-TRUE
  return(x)
}



