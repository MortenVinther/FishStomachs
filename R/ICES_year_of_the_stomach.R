#' Read ICES year-of-the-stomach data
#'
#' @param dir Directory with in_file
#' @param in_file Name or names of files with data on the ICES Year-of-the-stomach exchange format.
#' @param NODC_latin  CSV file with NODC and latin names. Default file system.file("NODC_latin.csv", package = "FishStomachs").
#' @param latin_prey_names Add latin names on preys from NODC
#' @param write_file Write file with data on exchange format, file name becomes "adjusted_"+in_file
#' @param verbose Show information on processing data.
#' @export
#' @return stomach data on exchange format
#'
convert_year_of_the_stomach<-function(dir,in_file,NODC_latin,latin_prey_names=TRUE,write_file=FALSE,verbose=FALSE) {

   month<-day<-time<-lat<-lon<-quadrant<-depth<-temp<-pred_w<-pred_l<-pred_ll<-pred_lu<-pred_cpue<-prey_ll<-NULL
  prey_lu<-prey_n<-prey_w<-NODC<-n<-species<-prey_nodc<-rectangle<-sample_number<-NULL
  pred_name<-country<-year<-quarter<-station<-country<-NULL

  if (missing(NODC_latin)) NODC_latin <- system.file("extdata","NODC_latin.csv", package = "FishStomachs")
  if (!file.exists(NODC_latin)) {cat('NODC_latin file: ',NODC_latin,' does not exist\n'); stop()}
  b <- dplyr::as_tibble(read.csv(file = NODC_latin, stringsAsFactors = FALSE)) %>%
       dplyr::select(species,NODC) %>% dplyr::distinct()
  stopifnot( dim(b %>% dplyr::group_by(NODC) %>% dplyr::summarise(n=n()) %>% dplyr::filter(n>1))[[1]]==0)  # no duplicated NODC

  out<-lapply(in_file,function(x) {
     # test x="RADIATA_915.dat"; dir=stomach_dir
    if (verbose) cat("Reading:",x,'\n')

     if (!file.exists(file.path(dir,x))) {cat('Input file: ',file.path(dir,x),' does not exist\n'); stop}
     a<-dplyr::as_tibble(read.fwf(file.path(dir,x),
              widths= c(2,2,1,4,10,5,3,4,3,6,2,2,4,1,4,5,3,3,4,5,2,4,4,7,3,3,3,3,10,4,4,8,6,1),
              colClasses=c("integer", "integer", "integer","character", "numeric","integer",  #24
                           "character","character","character","character","integer","integer", #48
                           "integer","integer","integer","integer","integer","integer","integer","integer", #73
                           "integer","integer","integer","integer","integer","integer","integer","integer", #112
                           "numeric","integer","integer","integer","integer","integer"),
              col.names=c("eco","year","quarter","rectangle","pred_nodc","sample_number",
                           "country","ship","samp_meth","station","month","day","time",
                           "quadrant","lat","lon","depth","temp",
                           "pred_l","pred_w","pred_age","pred_ll","pred_lu","pred_cpue",
                           "n_food","n_regur","n_skel","n_empty",
                           "prey_nodc","prey_ll","prey_lu","prey_w","prey_n","digest"),
               strip.white=TRUE,buffersize=10000,stringsAsFactors=FALSE
              ))
      a<-a %>% dplyr::mutate(month=ifelse(month==99,as.integer(NA),month),
                     day=ifelse(day==99,as.integer(NA),day),
                     time=ifelse(time==9999,as.integer(NA),time),
                     lat=ifelse(lat==9999,as.integer(NA),lat),
                     lon=ifelse(lon==99999,as.integer(NA),lon),
                     quadrant=ifelse(quadrant==9,as.integer(NA),quadrant),
                     depth=ifelse(depth==999,as.integer(NA),depth),
                     temp=ifelse(temp==999,as.integer(NA),temp),
                     pred_w=ifelse(pred_w==99999,as.integer(NA),pred_w),
                     pred_l=ifelse(pred_l==9999,as.integer(NA),pred_l),
                     pred_ll=ifelse(pred_ll==9999,as.integer(NA),pred_ll),
                     pred_lu=ifelse(pred_lu==9999,as.integer(NA),pred_lu),
                     pred_cpue=ifelse(pred_cpue==9999999 | pred_cpue==1,as.integer(NA),pred_cpue),
                     prey_ll=ifelse(prey_ll==9999,as.integer(NA),prey_ll),
                     prey_lu=ifelse(prey_lu==9999,as.integer(NA),prey_lu),
                     prey_n=ifelse(prey_n==999999,as.integer(NA),prey_n),
                     prey_w=ifelse(prey_w==99999999,as.integer(NA),prey_w))

      #NODC_latin<-NULL



      # put latin name on predator
      a<-dplyr::left_join(x=a,y=b,by = c("pred_nodc" = "NODC")) %>% dplyr::mutate(pred_name=species,species=NULL)

      # put latin name on preys
      if (latin_prey_names) {
        a<-dplyr::left_join(x=a,y=b,by = c("prey_nodc" = "NODC")) %>% dplyr::mutate(prey_name=species,species=NULL)
      } else a<- a %>%  dplyr::mutate(prey_name=prey_nodc)


      a<-a %>% dplyr::mutate(prey_w=prey_w/1000)  # weights from mg to g
    # cat('sample id should be changed, if full information on haul\n')
      a<- a %>% dplyr::mutate(eco=NULL,area=rectangle,fish_id=sample_number,sample_id=paste0(pred_name,"_",country,"_y:",year,"_q:",quarter,"_ID:",sample_number),
                       prey_w_meth='r',
                       year=dplyr::if_else(year<40,as.integer(year+2000),as.integer(year+1900)),
                       haul=station, prey_l=as.integer(NA),dataset='NS')


      if (write_file) write_csv(a,file=file.path(dir,paste0("adjusted_",x)))
      if (verbose) cat('File:',paste0("adjusted_",x),'is done\n')
      return(a)
  })

  invisible(out)
}

