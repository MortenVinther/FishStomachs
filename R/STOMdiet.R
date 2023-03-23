#' @title Convert STOMdiet into a data.frame
#' @param diet Diet dataset of class STOMdiet.
#' @method as.data.frame STOMdiet
#' @exportS3Method
#' @return STOMdiet object transformed to a "flat" data.frame.
as.data.frame.STOMdiet <- function(diet){
  d<-dplyr::left_join(diet[['PRED']],diet[['PREY']],by = c("key"))
  attr(d,'PRED')<-names(diet[['PRED']])
  attr(d,'PREY')<-names(diet[['PREY']])
  attr(d,'control')<-attr(diet,'control')
  att<-all_stom_attributes()
  for (aa in att)  attr(d,aa)<-attr(diet,aa)
  return(d)
}


# ---------------------------------------------------------------------------
#' Method to convert an object of class data.frame into an object of class STOMdiet.
#'
#' @title Convert a data.frame into a STOMdiet object
#' @param d Stomach data set on data.frame format. d must have been created from a call to of class as.data.frame().
#' @param new_pred_var Variable names to be added to the Predator data set in STOMdiet.
#'  which were added the call to as.data.frame()
#' @param new_prey_var Variable names to be added to the prey data set in STOMdiet
#' @return STOMdiet object
#' @export
#' @examples \dontrun{a<-as_STOMdiet(df); head(a)}
as_STOMdiet <- function(d,new_pred_var,new_prey_var){

  pn<-names(d)
  if (! missing(new_pred_var)) {
    for (v in new_pred_var) stopifnot("new_pred_var is is not in d"=v %in% pn)
    match(new_pred_var,pn)
  } else new_pred_var<-NULL

  pred<-unique(subset(d,select=unique(c(pn[pn %in% attr(d,'PRED')],new_pred_var))))

  if (! missing(new_prey_var)) {
    for (v in new_prey_var) stopifnot("new_prey_var is not in d"= v %in% pn)
  } else new_prey_var<-NULL

  prey<-subset(d,select=unique(c(pn[pn %in% attr(d,'PREY')],new_prey_var)))

  a<-list(PRED=pred,PREY=prey)
  class(a) <- "STOMdiet"
  attr(a,'control')<-attr(d,'control')
  att<-all_stom_attributes()
  for (aa in att)  attr(a,aa)<-attr(d,aa)
  return(a)
}



# ---------------------------------------------------------------------------
#' General method for sub-setting a STOMdiet object.
#'
#' A STOMdiet object contains two kinds of information:
#' \enumerate{
#' \item Between predator information (contained in component PRED)
#' \item Within prey information (contained in component PREY)
#' }
#' This method can subset over both kinds of information - at once.
#' The functionality is achieved by matching each of the subset critera
#' to the appropriate data component. When a match is found the subset is
#' performed on the component and the subsetting is continued to the next
#' subset criteria. The number of subset critera can be arbitrary and is
#' given through the \code{...} argument.
#' Code is copied from the DATRAS package
#'
#' @title Sub-setting a STOMdiet object.
#' @param x Take subset of this dataset.
#' @param ... One or more subset criteria.
#' @param na_rm Delete missing values.
#' @return The reduced dataset of class STOMdiet.
#' @method subset STOMdiet
#' @export
subset.STOMdiet <- function(x,...,na_rm=TRUE){
  # test  x<-a; na_rm=TRUE
  key<-NULL
  old.nrow <- sapply(x,nrow) # To test what parts of x have been changed
  args <- as.list(match.call()[-1][-1])
  #test print(args);  args<-list(); args[[1]]<-quote(year == 1997);   print(args)  #  from PRED
  #test print(args);  args<-list(); args[[1]]<-quote(prey_name == "Sprattus sprattus");   print(args)  #  from PREY

  vars <- lapply(args,all.vars)
  na2false <- function(x){
    if(!na_rm)return(x)
    if(!is.logical(x))stop("na2false requires logicals")
    x[is.na(x)] <- FALSE
    x
  }
  for(i in seq(args)){
    var <- vars[[i]]
    arg <- args[[i]]
    fit <- sapply(x,function(x)any(var %in% names(x)))
    if(!any(fit)){
      cat("Warning - no match found for:\n")
      print(arg)
    }
    if (fit['PRED']){
      x[['PRED']] <- x[['PRED']][na2false(eval(arg,x[['PRED']],parent.frame())),]
      keys <- x[['PRED']]$key
      x[['PREY']] <- subset(x[['PREY']],key %in% keys)
    } else if(fit['PREY']) {
      x[['PREY']] <- x[['PREY']][na2false(eval(arg,x[['PREY']],parent.frame())),]
      keys <- unique(x[['PREY']]$key)
      x[['PRED']] <- subset(x[['PRED']],key %in% keys)
    }
  }
  # Remove empty factor levels
  x[['PREY']]<-droplevels(x[['PREY']])
  x[['PRED']]<-droplevels(x[['PRED']])

  return(x)
}

# ---------------------------------------------------------------------------
#' Method to combine many datasets of the class STOMdiet
#'
#' This function combines STOMdiet objects by \code{dplyr::bind_rows} using each of the
#' two components of the objects.
#' The method is useful for combining diet data from a number of predator that do not use the same raising procedures.
#' If the same stomach_id is present within different datasets the method will stop.
#' If a variable name is only present in some of the datasets, the variable will be
#' added to the remaining datasets (filled with NA) and a warning will be triggered.
#' The control attributes from the first STOMdiet will be used for the resulting combined STOMdiet
#' The R-Code is modified from the DATRAS R-package (Thanks to Casper Berg)
#'
#' @title Combine multiple STOMdiet objects
#' @param ... STOMdiet objects to put together.
#' @return Combined dataset of class STOMdiet.
#' @examples \dontrun{ab<-c(a,b)}
#' @method c STOMdiet
#' @export
# ---------------------------------------------------------------------------
c.STOMdiet <- function(...){
   testUnique_sample_id <- function(args){
    x <- lapply(args,function(x)levels(x$sample_id))
    if(length(unique(unlist(x))) != length(unlist(x)))stop("Sample ids must be unique.")
  }
  args <- list(...)
  #test  args<-list(hom,mam)

  control<-attr(args[[1]],'control')
  control@dataSets<-unlist(lapply(args,function(x) attr(x,'control')@dataSets))

  testUnique_sample_id(args)
  args <- lapply(args,unclass) # because do_mapply dispatch on length
  # Add missing variable names with warning
  argnames <- Map(Map,list("names"),args)
  union <- function(...)unique(c(...))
  unionNames <- do.call("Map",c(list("union"),argnames))
  addMissingVariables <- function(x){
    out <- lapply(1:2,function(i){
      ans <- x[[i]]
      missingVariables <- setdiff( unionNames[[i]],names(ans) )
      if(length(missingVariables)>0){
        warning("Incomplete STOMobs? Missing ",names(x)[i],"-record(s): ",
                paste(missingVariables,collapse=", "),
                ". NA will be inserted.")
        if(nrow(ans)>0){
          ans[missingVariables] <- NA
        } else {
          for(ii in 1:length(missingVariables)) ans[,missingVariables[ii]] <- logical(0)
        }
      }
      ans
    })
    names(out) <- names(x)
    out
  }
  args <- lapply(args,addMissingVariables)
  args <- c(list("rbind"),args)
  ans <- do.call("Map",args)

  # Note that rbind drops all zero-row data.frames - hence levels of zero-length
  # factor will be dropped. Have to fix this:

  refactor_key_levels <- function(df){
    lev <- levels(df[['PRED']]$key)
    df[['PREY']]$key <- factor(df[['PREY']]$key,levels=lev)
    return(df)
  }
  ans <- refactor_key_levels(ans)


  class(ans) <- "STOMdiet"
  # assign attributes
  attr(ans,'control')<-control


  return(ans)
}


#' Print overview of diet data
#'
#' @param x Diet data of class STOMdiet.
#' @method print STOMdiet
#' @export
#'
#'
print.STOMdiet <- function(x){
  d1 <- x[['PRED']]
  d2 <- x[['PREY']]

  cat("Object of class 'STOMdiet'\n")
  cat("===========================\n")
  if ('n_tot' %in% colnames(d1)) cat("Number stomachs:",sum(d1$n_tot,na.rm=TRUE),"\n") else cat("Number stomachs with food :",sum(d1$n_food,na.rm=TRUE),"\n")
  cat("Predators:",paste0(levels(d1$pred_name),collapse = "; "),"\n")
  np<-nlevels(d2$prey_name)
  cat("Number of prey names:",np,"\n")
  pn<-levels(d2$prey_name)
  cat("Prey names:")
   for (n in (1:np)) { cat(paste0(pn[n],'; ')); if (n %% 4==0) cat('\n'); if ( (n %% 4==0) & n<np) cat('    ') }
  cat("Temporal strata:",paste0(levels(d1$stratum_time),collappse='; '),"\n")
  cat("\n")
}


#' Summary of diet
#' @param diet Diet data of class STOMdiet.
#' @param level Detail level.
#' @param digits Number of digits in output.
#' @param drop.unused.levels Drop unused factor levels.
#' @method summary STOMdiet
#' @export
summary.STOMdiet <- function(diet, level=1,digits=1,drop.unused.levels = FALSE){

  key<-prey_name<-prey_size<-prey_size_class<-prey_w<-NULL

  control<-attr(diet,'control')

  # calculate year and quarter from specifications
  diet[['PRED']]<-diet[['PRED']] %>% dplyr::mutate(year=as.integer(eval(control@strata_year_back)),quarter=as.integer(eval(control@strata_quarter_back)))

  d<-as.data.frame(diet)
  if (level==1) {
    cat("Number of stomachs by predator and year:\n")
    print(xtabs(n_tot ~ pred_name+year , data=diet[['PRED']]))
  } else if (level==2) {
    cat("Number of stomachs by predator and year:\n")
    ftable(xtabs(n_tot ~ pred_name+year+quarter , data=diet[['PRED']]))
  } else if (level==3) {
    a<-by(d,list(d$key),function(x) {
      x<-x %>% dplyr::group_by(key,prey_name,prey_size,prey_size_class) %>%
        dplyr::mutate(prey_w=sum(prey_w)) %>% dplyr::ungroup()
      xx<- x %>% dplyr::mutate(dplyr::across(where(is.factor), as.character))
      cat('Absolute weight:\n')
      cat(paste0('\n',xx[1,'pred_name'],', ',xx[1,'pred_size'],', ',xx[1,'year'],
                 ', Q:',xx[1,'quarter'],', n stomachs:',xx[1,'n_tot'],'\n'))
      cat('Absolute weight:\n')
      print(round(xtabs(prey_w ~ prey_name +prey_size, data=x,drop.unused.levels=drop.unused.levels),digits))})
  } else if (level==4) {
    a<-by(d,list(d$key),function(x) {
      x<-x %>%  dplyr::group_by(key) %>% dplyr::mutate(prey_w=prey_w/sum(prey_w)) %>% dplyr::ungroup()
      xx<- x %>% dplyr::mutate(dplyr::across(where(is.factor), as.character))
      cat('Relative weight in percentages:\n')
      cat(paste0('\n',xx[1,'pred_name'],', ',xx[1,'pred_size']
                  ,', ',x[1,'year'],', Q:',x[1,'quarter'],', n stomachs:',x[1,'n_tot'],'\n'))
      a<-xtabs(prey_w ~ prey_name +prey_size, data=x,drop.unused.levels=drop.unused.levels)
      a<-cbind(a,all=apply(a,1,sum))
      a<-rbind(a,all=apply(a,2,sum))
      a<-a*100
      print(round(a,digits))})
  }
}


#' Sum diet data with the same prey
#' @param diet Diet data of class STOMdiet.
#' @return Diet data of class STOMdiet summed for each combination of prey species and prey size.
#' @export
#'
 diet_sum<-function(diet){
   key<-prey_name<-prey_size<-prey_size_class<-prey_w<-NULL
    diet[['PREY']]<- diet[['PREY']] %>% dplyr::group_by(key,prey_name,prey_size,prey_size_class) %>% dplyr::summarise(prey_w=sum(prey_w)) %>% dplyr::ungroup()
  return(diet)
 }

#' Calculate diet data as relative weight
#' @param diet Diet data of class STOMdiet.
#' @return Diet data of class STOMdiet.
#' @export
  diet_relative<-function(diet){
   diet[['PREY']]<- diet[['PREY']] %>% dplyr::group_by(key) %>% dplyr::mutate(prey_w=prey_w/sum(prey_w)) %>% dplyr::ungroup()
   return(diet)
 }



#' Transform preys into "other"
#' @param diet Diet data of class STOMdiet.
#' @param preys_to_other Prey or preys to be added to the "other" prey group.
#' @export
 diet_prey_to_other<-function(diet,preys_to_other) {
   if (missing(preys_to_other)) stop('Please provide prey names for preys_to_other')
   control<-get_control(diet)
   mis_prey_len<-paste0(control@mis_l,'-',control@mis_l)
   other<-control@other
   mis_prey_size_class<-control@mis_size_class

   change<- diet[['PREY']]$prey_name %in% preys_to_other
   diet[['PREY']][change,'prey_name']<-other
   diet[['PREY']][change,'prey_size']<-mis_prey_len
   diet[['PREY']][change,'prey_size_class']<-mis_prey_size_class
   diet[['PREY']]<-diet[['PREY']] %>% dplyr::mutate_if(is.factor,forcats::fct_drop)
   diet<-diet_sum(diet)
   diet<-diet_relative(diet)
   return(diet)
 }
