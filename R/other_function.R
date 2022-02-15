# add additional information
get_species_info<-function(input_file){
  code<-l_w_a<-l_w_b<-number<-Species<-NULL
  a<-read.csv(file= input_file,stringsAsFactors=FALSE)
  a<-subset(a,select=c(Species,code,number,l_w_a,l_w_b))
  return(dplyr::as_tibble(a))
}

del_sample_ids<-function(stom,del_id) {
  sample_id<-NULL
  stom[['PRED']]<- dplyr::filter(stom[['PRED']], !( sample_id %in% del_id)) %>% dplyr::mutate(sample_id=forecats::fct_drop(sample_id))
  stom[['PREY']]<- dplyr::filter(stom[['PREY']], !( sample_id %in% del_id))
  lev <- levels(stom[['PRED']]$sample_id)
  stom[['PREY']]$sample_id <- factor(stom[['PREY']]$sample_id,levels=lev)
  return(stom)
}

cleanup<-function(){for(i in dev.list()) if (names(dev.off())=='null device') break()}
