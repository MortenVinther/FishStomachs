
# ---------------------------------------------------------------------------
#' Method to convert an object of class STOMobs into a data frame (and tibble).
#' Information on sampling and predators are copied to each record of prey information.
#' @title Convert a STOMobs into a data.frame
#' @param stom Stomach data set of class STOMobs.
#' @param add_empty Insert prey "other" with prey weight (prey_w) =0 for samples with only empty stomachs.
#' @return STOMobs object transformed to a "flat" data frame.
#' @method as.data.frame STOMobs
#' @seealso  \code{\link[FishStomachs]{as_STOMobs}}
#' @exportS3Method
#' @examples \dontrun{a<-as.data.frame(stom); a; as_STOMobs(a)}
as.data.frame.STOMobs <- function(stom,add_empty=TRUE){

   a<-dplyr::left_join(stom[['PRED']],stom[['PREY']],by = c("sample_id", "fish_id"))
  if (add_empty) {
    control<-attr(stom,'control')
    min_prey_length<-control@min_prey_length
    mis_l<-control@mis_l
    other<-control@other
    mis_size_class<-control@mis_size_class
    mis_ll<-paste(mis_l,mis_l,sep='-')
    cnames<-colnames(a)
    crit<-is.na(a$prey_name)
    if (any(crit)) {
      a[crit,'prey_name']<-other
      if ('prey_size' %in% cnames) a[crit,'prey_size']<-mis_ll
      if ('prey_size_class' %in% cnames) a[crit,'prey_size_class']<-mis_size_class
      a[crit,'prey_w']<-0
    }
  }

  attr(a,'PRED')<-names(stom[['PRED']])
  attr(a,'PREY')<-names(stom[['PREY']])
  attr(a,'control')<-attr(stom,'control')
  att<-all_stom_attributes()
  for (aa in att)  attr(a,aa)<-attr(stom,aa)
  return(a)
}


# ---------------------------------------------------------------------------
#' Method to update variables names in the attributes for STOMobs.
#' The attributes with variable names are updated for the PRED and PREY data frame.
#' @title Update attributes with added variables
#' @param stom Stomach data set of class STOMobs.
#' @return  Stomach data set of class STOMobs with updated attributes.
#' @method update STOMobs
#' @exportS3Method
#' @examples \dontrun{stom<-update(stom); stom}
update.STOMobs <- function(stom){
  attr(stom,'PRED')<-names(stom[['PRED']])
  attr(stom,'PREY')<-names(stom[['PREY']])
  return(stom)
}



# ---------------------------------------------------------------------------
#' Method to convert an object of class data.frame into an object of class STOMobs.
#'
#' @title Convert a data.frame into a STOMobs object
#' @param d Stomach data set on data.frame format. d must have been created from a call to of class as.data.frame().
#' @param new_pred_var Variable names to be added to the predator (PRED) data set in STOMobs.
#'  which were added the call to as.data.frame()
#' @param new_prey_var Variable names to be added to the prey (PREY) data set in STOMobs
#' @return STOMobs object
#' @export
#' @examples \dontrun{a<-as_STOMobs(df); a}
as_STOMobs <- function(d,new_pred_var,new_prey_var){

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
  class(a) <- "STOMobs"
  attr(a,'control')<-attr(d,'control')
  att<-all_stom_attributes()
  for (aa in att)  attr(a,aa)<-attr(d,aa)
  return(a)
}



# ---------------------------------------------------------------------------
#' General method for sub-setting a STOMobs object.
#'
#' A STOMobs object contains two kinds of information:
#' \enumerate{
#' \item Between predator information (contained in component PRED)
#' \item Within prey information (contained in component PREY)
#' }
#' This method can subset over both kinds of information - at once.
#' The functionality is achieved by matching each of the subset criteria
#' to the appropriate data component. When a match is found the subset is
#' performed on the component and the subsetting is continued to the next
#' subset criteria. The number of subset criteria can be arbitrary and is
#' given through the \code{...} argument.
#' Code is copied from the DATRAS package
#'
#' @title Sub-setting a STOMobs object.
#' @param x Take subset of this dataset.
#' @param ... One or more subset criteria.
#' @param na_rm Discard missing values in subset criterion?
#' @return subset of dataset of class STOMobs.
#' @method subset STOMobs
#' @export
#' @examples \dontrun{subset(ns,pred_name=='Cod')}
subset.STOMobs <- function(x,...,na_rm=TRUE){

  # test  x<-a; na_rm=TRUE
  old.nrow <- sapply(x,nrow) ## To test what parts of x have been changed
  args <- as.list(match.call()[-1][-1])

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
      lev <- levels(factor(x[['PRED']]$sample_id))
      x[['PREY']] <- x[['PREY']][x[['PREY']]$sample_id %in% lev,]
      lev <- levels(factor(x[['PRED']]$fish_id))
      x[['PREY']] <- x[['PREY']][x[['PREY']]$fish_id %in% lev,]

    } else if(fit['PREY']) {
      x[['PREY']] <- x[['PREY']][na2false(eval(arg,x[['PREY']],parent.frame())),]
      lev <- levels(factor(x[['PREY']]$sample_id))
      x[['PRED']] <- x[['PRED']][x[['PRED']]$sample_id %in% lev,]
      lev <- levels(factor(x[['PREY']]$fish_id))
      x[['PRED']] <- x[['PRED']][x[['PRED']]$fish_id %in% lev,]

    }
  }
  ## Remove empty factor levels
  new.nrow <- sapply(x,nrow)
  changed <- old.nrow!=new.nrow

  ## re-factor all, but not sample_id and fish_id
  refactorPREY <- function(df){
    i <- sapply(df,is.factor)
    i <- i & ((names(df)!="sample_id") | (names(df)!="fish_id"))
    df[i] <- lapply(df[i],factor)
    df
  }
  if(changed['PREY']) x[['PREY']] <- refactorPREY(x[['PREY']])

  ##  refactor all of PRED.
  ##  refactor sample_id and fish_id in PREY consistently
  refactor <- function(df){
    i <- sapply(df[['PRED']],is.factor)
    df[['PRED']][i] <- lapply(df[['PRED']][i],factor)
    lev <- levels(df[['PRED']]$sample_id)
    df[['PREY']]$sample_id <- factor(df[['PREY']]$sample_id,levels=lev)
    lev <- levels(df[['PRED']]$fish_id)
    df[['PREY']]$fish_id <- factor(df[['PREY']]$fish_id,levels=lev)

    df
  }
  if(changed['PRED'])x <- refactor(x)

  return(x)
}

#' Print STOMobs
#' @param x Object of class STOMobs
#' @param show_attributes Show the attributes for for the objects.
#' @method print STOMobs
#' @export
print.STOMobs <- function(x,show_attributes=TRUE){
  d1 <- x[['PRED']]
  d2 <- x[['PREY']]

  cat("Object of class 'STOMobs'\n")
  cat("===========================\n")
  cat("Number of samples:",nlevels(d1$sample_id),"\n")
  if ('n_tot' %in% colnames(d1)) cat("Number stomachs:",sum(d1$n_tot,na.rm=TRUE),"\n") else cat("Number stomachs with food :",sum(d1$n_food,na.rm=TRUE),"\n")
  #cat("Predators:",levels(d1$pred_name),"\n")
  cat("Predators:",paste(levels(d1$pred_name),collapse=", "),"\n")
  cat("Number of prey names:",nlevels(d2$prey_name),"\n")
  cat("Years:",sort(unique(d1$year)),"\n")
  cat("\n")
  if (show_attributes) lapply(all_stom_attributes(),function(xx){aa<-attr(x,which=xx); cat(paste0(aa,ifelse(aa,':  ',': '),xx,'\n')) })
}


length.STOMobs <- function(x)nrow(x[['PRED']])



#' Summary of number of stomachs
#' @method summary STOMobs
#' @param x Diet data of class STOMdiet.
#' @param level Level of details, 1 or 2.
#' @examples \dontrun{summary(s,level=2)}
#' @export
summary.STOMobs <- function(x, level=1){
  stopifnot('n_tot' %in% names(x[['PRED']]))
  if (level==1) {
    cat("Number of stomachs by predator and year:\n")
    print(xtabs(n_tot ~ pred_name+year , data=x[['PRED']]))
  } else if (level==2) {
      n<-names(x[['PRED']])
      vars<-c("n_food", "n_empty","n_skel","n_regur","n_tot")
      vars<-vars[vars %in% n]
      cat("Number of stomachs by predator and year:\n")
      ftable(xtabs(as.formula(paste("cbind(",paste(vars,collapse=','),")", "~ pred_name+year" )), data=x[['PRED']]))
  }
}

## ---------------------------------------------------------------------------
#' Method to combine many datasets
#'
#' This function combines STOMobs objects by \code{dplyr::bind_rows} using each of the
#' two components of the objects.
#' The method is useful when reading large amounts of data in small chunks.
#' If the same stomach_id is present within different datasets the method will stop.
#' If a variable name is only present in some of the datasets, the variable will be
#' added to the remaining datasets (filled with NA) and a warning will be triggered.
#' The control attributes from the first STOMobs will be used for the resulting combined STOMobs
#' The R-Code is modified from the DATRAS R-package (Thanks to Casper Berg)
#'
#' @title Combine multiple STOMobs objects
#' @param ... STOMobs objects to put together.
#' @return Combined dataset of class STOMobs.
#' @examples \dontrun{ab<-c(a,b)}
#' @method c STOMobs
#' @export
## ---------------------------------------------------------------------------
c.STOMobs <- function(...){
  fish_id<-sample_id<-NULL
  testUnique_sample_id <- function(args){
    x <- lapply(args,function(x)levels(x$sample_id))
    if(length(unique(unlist(x))) != length(unlist(x)))stop("Sample ids must be unique.")
  }
  args <- list(...)

  status<-lapply(all_stom_attributes(),function(x)attr(args[[1]],x))

  control<-attr(args[[1]],'control')
  control@dataSets<-unlist(lapply(args,function(x) attr(x,'control')@dataSets))

  testUnique_sample_id(args)
  args <- lapply(args,unclass) ## because do_mapply dispatch on length
  ## Add missing variable names with warning
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

  ## Note that rbind drops all zero-row data.frames - hence levels of zero-length
  ## factor will be dropped. Have to fix this:

  refactor_sample_fish_id_levels <- function(df){
    lev <- levels(df[['PRED']]$sample_id)
    df[['PREY']]$sample_id <- factor(df[['PREY']]$sample_id,levels=lev)
    lev <- levels(df[['PRED']]$fish_id)
    df[['PREY']]$fish_id <- factor(df[['PREY']]$fish_id,levels=lev)
    return(df)
  }
  ans <- refactor_sample_fish_id_levels(ans)


  class(ans) <- "STOMobs"

  ## assign attributes
  status<-unlist(status)
  i<-1
  for (at in all_stom_attributes()) {
    attr(ans, at) <- status[i]
    i<-i+1
  }
  attr(ans,'control')<-control

  # is not necessary !, already done by test Unique_sample_id
  b <- check_unique_sample_id(ans)
  if (dim(b)[[1]] > 0) {
    b<-b %>% dplyr::arrange(sample_id,fish_id)
    print(data.frame(b))
    stop("Duplicated combinations of sample_id and fish_id have been found, please check!")
  }
  return(ans)
}


