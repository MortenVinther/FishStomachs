
#' Group prey species names.
#'
#' Prey species are renamed and re-grouped into larger groups of prey, with summed prey weights. The simplest approach maintain the species names provided
#' by \code{keep_prey_names} and rename all other preys into "other". A more complex method uses a key based on the NODC to split (see \code{NODC_split}).
#' @param stom Stomach contents data of class STOMobs.
#' @param keep_prey_names Names of preys to be kept unchanged. Remaining preys are renamed to "other" and their prey sizes are changed to missing.
#' @param sum_other_food Sum  stomach contents of "other" preys.
#' @param NODC_split Input file with the fields XXXXX
#' @param show_allocations If true the allocation of prey names into groups are shown, when the \code{NODC_split} is used.
##' in the control object are renamed into "other".
#'
#' @return Stomach contents data of class STOMobs. Prey sizes and number of preys as set to missing for preys in the "other" food group.
#' @export
#'
group_prey_species <- function(stom, keep_prey_names,sum_other_food=TRUE,NODC_split,show_allocations=FALSE ) {
#debug  stom=ns; keep_prey_names<-c("Gadus morhua","Clupea harengus", "Sprattus sprattus"); sum_other_food=TRUE;show_allocations=TRUE;
  fish_id<-prey_group<-prey_name<-prey_w<-sample_id<-NULL
  if (missing(keep_prey_names) & missing(NODC_split)) stop('You have to supply either the parameter keep_prey_names or NODC_split.\n ')

  control<-attr(stom,'control')
  min_prey_length<-control@min_prey_length
  mis_l<-control@mis_l
  other<-control@other

  st<-stom[['PREY']]

  do_other<-function(oth) {
    if (use_prey_size)  oth<-oth %>% dplyr::mutate(prey_size=mis_prey_size,prey_size_class=0L,prey_l_mean=NA)
    if (!use_prey_size) oth<-oth %>% dplyr::mutate(prey_l=NA,prey_ll=mis_l,prey_lu=mis_l)
    oth<- dplyr::mutate(oth, digest=9L,prey_n= NA)
    return(oth)
  }
  if (attr(stom,all_stom_attributes()["prey_w_id"])==FALSE & "prey_w_meth" %in% colnames(st)) stop('ERROR: You have to run prey_w_from_pooled_weight() first \n')

  if ("prey_size" %in% colnames(st)) {
    use_prey_size<-TRUE
    mis_prey_size <- factor(paste(mis_l,mis_l,sep='-'),levels=levels(st$prey_size))
  } else use_prey_size<-FALSE


  if (missing(NODC_split)) {
    cat('Grouping of prey species is done on the basis of keep_prey_names:',paste(keep_prey_names,collapse=', '),'\n')
    st$prey_name<-as.character(st$prey_name)
     id_prey<- st$prey_name %in% keep_prey_names
    oth<-st[!id_prey,] %>% dplyr::mutate(prey_name=other)
    remains<-st[id_prey,]

    if (sum_other_food) {
      oth<-oth %>% dplyr::group_by(sample_id,fish_id,prey_name) %>% dplyr::summarise(prey_w=sum(prey_w)) %>%dplyr::ungroup()
    }
    oth<-do_other(oth)
    st<-dplyr::bind_rows(remains,oth) %>% dplyr::mutate(prey_name=fct_explicit_na(prey_name))
    stom[['PREY']]<-st
  } else {
    named<-st$prey_nodc %in% dplyr::filter(NODC_split,named)$First
    st_named<-st[named,] %>% dplyr::mutate(prey_name=as.character(prey_name))
    #dplyr::select(st_named,prey_name,prey_nodc) %>% dplyr::distinct()

    st<-st[!named,]
    named<-dplyr::filter(NODC_split,!named) %>% dplyr::select(-named)
    a<-by(named,list(named$species_group,named$First,named$Last),function(x){
      dplyr::filter(st,between(prey_nodc,x$First,x$Last)) %>% dplyr::mutate(prey_group=as.character(x[1,'species_group']))
    })
    st2<-do.call(dplyr::bind_rows,a)
    if (show_allocations) print(xtabs(~prey_name+prey_group,data=st2) ,max=10000)
    if (sum_other_food & dim(dplyr::filter(st2,prey_group==other))[[1]]>0) {
      oth<-dplyr::filter(st2,prey_group==other) %>% dplyr::group_by(sample_id,fish_id,prey_group) %>% dplyr::summarise(prey_w=sum(prey_w)) %>%dplyr::ungroup()
      oth<-do_other(oth)
      remains<-dplyr::filter(st2,prey_group!=other)
      st2<-dplyr::bind_rows(remains,oth) %>% dplyr::mutate(prey_nodc=-9)
    }

    stom[['PREY']]<-dplyr::bind_rows(st_named, dplyr::mutate(st2,prey_name=prey_group, prey_group=NULL))%>%  dplyr::mutate(prey_name=fct_explicit_na(prey_name))

    dplyr::filter(stom[['PREY']],prey_name=='unknown')
  }
  return(stom)
}

