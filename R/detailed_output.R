
#' Extract a subset stomach contents data (of an object of class STOMobs) from criteria provided by control@detailed_tst_criteria.
#' This function is useful for tracking a subset of STOMobs though series of data manipulations.
#' @title Extract a subset from test criteria
#' @param stom Stomach data set of class STOMobs.
#' @return STOMobs object.
#' @examples  \dontrun{make_criteria_set(stom)}
#' @export
make_criteria_set<-function(stom){
  control<-attr(stom,'control')
  st_names<-unique(c(colnames(stom[['PRED']]),colnames(stom[['PREY']])))
  x<-control@detailed_tst_criteria
  vars<-names(x)
  for (i in (1:length(vars))) if (vars[i] %in% st_names) {
    eval(substitute(stom<-subset(stom, subs),list(subs=x[[i]])))
  }
  return(stom)
}

#' Make detailed summary output from a STOMobs object using a subset of data defined in the control attributes.
#'
#' @title Extract a subset from test criteria
#' @param stom Stomach data set of class STOMobs.
#' @param append logical, append to output file?
#' @param to_screen logical, write results on the screen.
#' @param write_criteria logical, write criteria for sub-setting.
#' @param label text included in output.
#' @param digits number of significant digits in output.
#' @param rel_weight logical for relative or absolute weight.
#' @param write_criteria Write selection  criteria.
#' @param use_criteria Use the criteria for a subset stored in the control attribute is selecting data.
#' @param show_mis_length Include preys with missing length information.
#' @param transpose Transpose output table.
#' @param by_sample_id Output by sample id.
#' @return STOMobs object.
#' @examples  \dontrun{do_detailed_output(stom)}
#' @export
#'
#'
do_detailed_output <-function(stom,append=TRUE,to_screen=FALSE,label,digits=1,rel_weight=FALSE,write_criteria=FALSE,use_criteria=TRUE,show_mis_length=TRUE,transpose=FALSE,by_sample_id=FALSE) {
  #test  stom<-s; digits=1; rel_weight=FALSE; write_criteria=TRUE; append=FALSE;transpose=TRUE;by_sample_id=TRUE; show_mis_length=FALSE; use_criteria=FALSE

  control<-attr(stom,'control')
  if (!use_criteria) by_sample_id<-FALSE
  if (!control@detailed_tst_output & use_criteria) return('no output requested (option detailed_tst_output=FALSE)')

   if (use_criteria) st<-data.frame(as.data.frame(make_criteria_set(stom))) else st<-data.frame(as.data.frame(stom))
   if (!"prey_ll" %in% colnames(st)) st$prey_ll<-st$prey_size
   if(show_mis_length) if (is.numeric(st$prey_ll)) st[is.na(st$prey_ll),'prey_ll']<-control@mis_l

   if (!to_screen) sink(file=control@detailed_tst_file,append=append)

  if (write_criteria  & use_criteria) {
    crit<-control@detailed_tst_criteria
    vars<-names(crit)
    st_names<-colnames(st)
    crit_out<-''
    for (i in (1:length(vars))) if (vars[i] %in% st_names)   crit_out=paste(crit_out,crit[[i]],sep=',')
    cat('Criterium:\n ',gsub('\\"','"',crit_out),'\n')
  }

  table_it<-function(x){
    if (by_sample_id) {
       cat('\n')
      if ('stratum_area' %in% colnames(x)) cat('stratum_area:',as.character(x[1,'stratum_area']))
      if ('stratum_sub_area' %in% colnames(x)) cat(' stratum_sub_area:',as.character(x[1,'stratum_sub_area']))
      if ('stratum_time' %in% colnames(x)) cat(' stratum_time:',as.character(x[1,'stratum_time']))
      cat( " sample_id:",as.character(x[1,"sample_id"])," fish_id:",as.character(x[1,"fish_id"]),"\n")
      if ('n_tot' %in% colnames(x)) cat("Number of stomachs ('n_tot'):",x[1,'n_tot'],"\n")
    }
    x$prey_name<-as.character(x$prey_name)
    x<-tapply(x$prey_w,list(x$prey_name,x$prey_ll),sum,na.rm=TRUE)
    x<-rbind(x,all=colSums(x,na.rm=TRUE))
    x<-cbind(x,all=rowSums(x,na.rm=TRUE))
    if (rel_weight) x<-x/x[dim(x)[[1]],dim(x)[[2]]]*100
    if (transpose) x<-t(x)
    print(round(x,digits))
    invisible(NULL)
  }

  if (!missing(label)) cat('\n\n',label,'\n')

  if (by_sample_id) by(st,list(st$sample_id,st$fish_id),table_it) else table_it(st)
  if (!to_screen) sink()
  if (to_screen) invisible(NULL) else return(paste('Output',ifelse(append,'appended',''),' to file:',control@detailed_tst_file))
}


do_detailed_output_diet <-function(diet,append=TRUE,to_screen=FALSE,label,digits=1,write_criteria=FALSE,transpose=FALSE) {
  #test  diet<-bb; digits=1; write_criteria=TRUE; append=FALSE;transpose=TRUE; to_screen=FALSE
  control<-attr(diet,'control')
  if (!control@detailed_tst_output) return('no output requested (option detailed_tst_output=FALSE)')

  st<-data.frame( as.data.frame(make_criteria_set(diet)) )

  if (!to_screen) sink(file=control@detailed_tst_file,append=append)

 if (write_criteria) {
    crit<-control@detailed_tst_criteria
    vars<-names(crit)
    st_names<-colnames(st)
    crit_out<-''
    for (i in (1:length(vars))) if (vars[i] %in% st_names)   crit_out=paste(crit_out,crit[[i]],sep=', ')
    cat('Criterium:\n ',gsub('\\"','"',crit_out),'\n')
  }

  table_it<-function(x){
    cat('\n')
    cat(' stratum_time:',as.character(x[1,'stratum_time']),
        as.character(x[1,'pred_name']),
        as.character(x[1,'pred_size']),'\n')

    cat("Number of stomachs ('n_tot'):",x[1,'n_tot'],"\n")

    x<-tapply(x$prey_w,list(x$prey_name,x$prey_size),sum,na.rm=TRUE)
    x<-rbind(x,all=colSums(x,na.rm=TRUE))
    x<-cbind(x,all=rowSums(x,na.rm=TRUE))
    x<-x*100
    #x<-x/x[dim(x)[[1]],dim(x)[[2]]]*100
    if (transpose) x<-t(x)
    print(round(x,digits))
    invisible(NULL)
  }
  if (!missing(label)) cat('\n\n',label,'\n')

  aa<-by(st,list(st$stratum_time,st$pred_name,st$pred_size), table_it)
  if (!to_screen) {sink(); return(paste('Output',ifelse(append,'appended',''),' to file:',control@detailed_tst_file)) } else invisible(NULL)
 }


