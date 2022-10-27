#' Define length classes for predator and preys.
#'
#' This function makes length classes defined from a lower and an upper value of length (or any other size measurement)
#' The definition of length classes is given in an input file \code{inp_file},
#' @details see this table
#'  \tabular{rrrrrr}{
#' \strong{l} \tab \strong{Species} \tab \strong{y1} \tab \strong{y2} \tab \strong{q1} \tab \strong{q2} \cr
#'    50 \tab  ALL     \tab  1991 \tab  2013 \tab  1  \tab    4 \cr
#'    60 \tab  ALL     \tab  1991 \tab  2013 \tab  1  \tab    4 \cr
#'    70 \tab  ALL     \tab  1991 \tab  2013 \tab  1  \tab    4 \cr
#'    80 \tab  ALL     \tab  1991 \tab  2013 \tab  1  \tab    4 \cr
#'}
#' where
#'  l is the length used to define length classes, e.g. the first two lines will produce the sizes "050-059" and "060-69".
#'  Species is the species name for the length classes. If Species=ALL, all the species will get the same lenngth classes
#'  y1 and y2 are the year range for which the length  classes will be used.
#'  q1 and q2 are the year range for which the length  classes will be used.
#'
#' The defined length classes are made for all combinations of Species, y1 to y1, q1 to q2.
#'
#' @title Put size classes on predator from observed length.
#' @param inp_dir Input directory for \code{inn_file}.
#' @param inp_file File name with the configuration of length classes with the format as outlined above. The file must be a CSV file
#' @param max_l Upper length in the last "largest" length class.
#' @param write_output Flag for saving the length classes defined on \code{out_file}
#' @param out_file Output file with defined length classes.
#' @param minus_one Value to define upper length in length class, e.g. \code{minus_one}= -1 will provide length classes "050-059" and "060-69" for l 50, 60 and 70.
#' @param same_no_all_data Logical, should all size classes have the same numbering irrespective of year and quarter?
#' @return defined length classes.
#' @export
#' @examples \dontrun{ll<-make_length_classess(inp_dir=system.file('extdata', package = 'FishStomachs'),inp_file='length_classes_config.csv')}
make_length_classess<-function(inp_dir=".",inp_file='length_classes_config.csv',max_l=9999,write_output=TRUE,out_file='length_classes.csv',minus_one= -1,same_no_all_data=FALSE) {
  l1<-NULL
  max_l<-as.integer(max_l)
widthl<-4

l<-read.csv(file=file.path(inp_dir,inp_file),stringsAsFactors=FALSE) %>% unique() %>%
 dplyr::group_by(Species,y1,y2,q1,q2) %>% dplyr::mutate(l2=dplyr::lead(l),no=1:dplyr::n()) %>% rename(l1=l) %>% ungroup()
l[is.na(l$l2),'l2']<-max_l


l2<-by(l,list(l$Species,l$y1,l$y2,l$q1,l$q2,l$no),function(x){
  a<-expand.grid(year=x$y1:x$y2,quarter=x$q1:x$q2)
  a<-data.frame(Species=x$Species,a,l1=x$l1,l2=x$l2,no=x$no)
  a
})

l2<-do.call(rbind,l2)
l2$Species<-as.character(l2$Species)
l2<-l2[order(l2$Species,l2$year,l2$quarter,l2$no),]
l2[l2$l2 !=max_l,'l2']<-as.integer(l2[l2$l2 != max_l,'l2']+minus_one)
l2$group<-paste(formatC(l2$l1, width=widthl,flag='0'),formatC(l2$l2, width=widthl,flag='0'),sep='-')

if (same_no_all_data) {
  l3<-l2 %>% dplyr::select(Species,group) %>% dplyr::distinct() %>% dplyr::group_by(Species) %>% dplyr::mutate(nno=dplyr::row_number()) %>%ungroup()
  l2<-dplyr::right_join(l3,l2,by = c("Species", "group")) %>% dplyr::mutate(no=nno,nno=NULL) %>%
      dplyr::arrange(Species,year,quarter,group)
}

if (write_output) write.csv(l2,file=file.path(inp_dir,out_file),row.names = FALSE)
return(dplyr::as_tibble(l2))
}


