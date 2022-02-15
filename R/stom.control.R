### class ######################################################################


#' Validation of control object used for stomach data compilation.
#'
#' @param object Object of class STOMcontrol
#'
validSTOMcontrol <- function(object) {
  if (FALSE) {
    if (!is.expression(object@stomach_format))
      return(paste(object@stomach_format, "is not an expression"))
    fl <- eval(object@stomach_format)
    if (!is.character(fl))
      return(paste(object@stomach_format, "is not a file name"))
    if (!file.exists(fl))
      return(paste(
        "File:",
        fl,
        "for definition of stomach contents data does not exist."
      ))
  }
  # Everything is fine
  return(TRUE)
}


#' An s4 class for Control object for stomach compilation
#'
#' @slot name character. Name of control object.
#' @slot stomach_format expression. \code{expression} to get file name for file defining stomach contents data. Default: expression(system.file("stomach_format.csv", package = "FishStomachs"))
#' @slot stomach_dir characters. Directory name for directory with stomach data on exchange format.
#' @slot dataSets vector of characters. File name or file names for data on exchange format.
#' @slot years vector. Year of data to be used.
#' @slot quarters vector. Quarters of the year ( or similar time split) to be used.
#' @slot strata_area_exp expression for defining strata area.
#' @slot strata_sub_area_exp expression for defining sub_area.
#' @slot strata_time_exp expression for defining temporal strata.
#' @slot strata_year_back    "expression" (R-code) for getting year information out of diet data, e.g. \code{expression(as.numeric(substr(stratum_time,1,4)))}.
#' @slot strata_quarter_back "expression" (R-code) for getting quarter information out of diet data.
#' @slot sel_preys vector.
#' @slot year_quarter_strata vector.
#' @slot weighting_by_nstom logical.
#' @slot combine_starta_from_relative_stomach_content logical.
#' @slot min_prey_length integer.
#' @slot max_l integer. Number used for the maximum length, e.g. 9998L
#' @slot mis_l integer. Number used for missing length, e.g. 9999L
#' @slot other character. Name for the "Other food", which is prey items not considered as individual prey species for further analysis, e.g. "other".
#' @slot mis_size_class integer. Number for size class where the size is unknown, e.g. 0L
#' @slot model_options list. Options for producing input values to multi species models as SMS.
#' @slot do_test_output logical.
#' @slot do_test_output_file logical.
#' @slot tst_file character.
#' @slot detailed_tst_output logical.
#' @slot detailed_tst_file character.
#' @slot detailed_tst_criteria list.
#' @export
#' @return a control object
#'
#' @examples \dontrun{control<-new("STOMcontrol",
#'       name='Baltic cod',
#'       stomach_dir=file.path(stom_data_dir,'baltic'),
#'       dataSets= c("balt_stom_new_formatNO_new_var.csv"))
#'       }
setClass(
  "STOMcontrol",
  slots = c(
    name = "character",
    stomach_format = "expression",

    ## data_included
    stomach_dir = "character",
    dataSets = "vector",
    years = "vector",
    quarters= "vector",
    predators= "vector",

    ## Stratification
    strata_area_exp = "expression",
    strata_sub_area_exp = "expression",
    strata_time_exp = "expression",
    strata_year_back = "expression",
    strata_quarter_back = "expression",

    ## Calculation of mean strata stomach contents,
    sel_preys = "vector",
    min_prey_length = "integer",
    calc_sub_strata="list",
    calc_strata="list",
    calc_total="list",

    ## Options for creating stomach contents file to SMS
    model_options ="list",

    ## Constants used
    max_l = "integer",
    mis_l = "integer",
    other = "character",
    mis_size_class = "integer",

    ## options for test output
    do_test_output = "logical",
    do_test_output_file = "logical",
    tst_file = "character",

    ## options for detailed test output
    detailed_tst_output = "logical",
    detailed_tst_file = "character",
    detailed_tst_criteria = "list"
  ),
  prototype = list(
    name = "myDataSet",
    stomach_format = expression(system.file("stomach_format.csv", package = "FishStomachs")),

    ## data_included
    stomach_dir = "mydir",
    dataSets = as.vector("myfile.csv", mode = "character"),
    predators=c('predators'),
    years = 1900L:1910L,
    quarters = 1L:4L,


    ## Stratification
    strata_time_exp = expression(paste0(year, '-Q', quarter)),
    strata_area_exp = expression(paste0('SD-', area)),
    strata_sub_area_exp = expression(paste0('SD-', area)),
    strata_year_back = expression(as.numeric(substr(stratum_time,1,4))),
    strata_quarter_back = expression(as.numeric(substr(stratum_time,7,8))),

    ## Calculation of mean stomach contents,
    sel_preys = as.vector("prey1", mode = "character"),
    min_prey_length = 50L,
    calc_sub_strata=list(
      relative_weight=FALSE,
      weighting_factor=expression(n_tot),
      import_weigthing_factor_file=NA
    ),
    calc_strata=list(
      relative_weight=FALSE,
      weighting_factor=expression(n_tot),
      import_weigthing_factor_file=NA
    ),
    calc_total=list(
      relative_weight=FALSE,
      weighting_factor=expression(n_tot),
      import_weigthing_factor_file=NA
    ),

    ## Options for creating stomach contents file to SMS
    model_options = list(
      minimum_stom = 1.0E-4, # minimum stomach contents proportion to be used by SMS
      insert_mid_value = TRUE,
      mid_value = 1.0E-4,  # stomach contents value inserted for missing prey lengths within an observed range
      insert_tails = FALSE,
      tail_value = 1.0E-4,

      ## Options for creating ALK file to SMS
      first_age = 0L,
      first_quarter_first_age = 3L,
      minimum_alk = 1.0E-4   # minimum proportion for ALK to be used by SMS
    ),

    ## Constants used
    max_l = 9999L,
    mis_l = 9999L,
    other = "other",
    mis_size_class=0L,

      ####### options for test output
    do_test_output = FALSE,
    do_test_output_file = FALSE,
    tst_file = "my_detailed.file",

    detailed_tst_output = FALSE,
    detailed_tst_file = "my_detailed.file",
    detailed_tst_criteria = list(
      year = expression(year == 1974L),
      quarter = expression(quarter == 1L),
      area = expression(area == 26),
      pred_l = expression(pred_l %in% 400:499),
      pred_size = expression(pred_size == '0400-0499'),
      pred_name = expression(pred_name == 'Gadus morhua'),
      stratum_time = expression(stratum_time == '1974-Q1'),
      stratum_time = expression(stratum_area == 'SD-26'),
      my_output_variables = c(
        "dataset",
        "country",
        "area",
        "year",
        "quarter",
        "sample_id",
        "fish_id",
        "pred_name",
        "pred_l",
        "pred_ll"
      )
    )
  ) ,  validity = validSTOMcontrol
)
### End class ###########################################################

#' Print control object
#' @param x object of class STOMcontrol or STOMobs.
#' @param show Show values for selected group of options. Possible groups are 'general','data_used','stratification','calc_diet','model_output','constants','test_output' and 'detailed_test_output'.
#' @export
setMethod("print", signature(x = "STOMcontrol"), function(x,show=c('general','data_used','stratification','calc_diet','model_output','constants','test_output','detailed_test_output')){

  shows<-c('general','data_used','stratification','calc_diet','model_output','constants','test_output','detailed_test_output')

  a<-slotNames(new("STOMcontrol"))

  b<-c(rep(shows[1],2),rep(shows[2],5),rep(shows[3],5),rep(shows[4],5), rep(shows[5],1),rep(shows[6],4),
   rep(shows[7],3) ,rep(shows[8],3))

  names(b)<-a

  n.<-slotNames(x)
  for (s in show) {
    cat('\n#### group:', s,'\n')
    for (xx in n.) if (b[xx]==s) {
      cl<-class(slot(x,xx))
      cat(xx,": ",sep="")
      cat(str(slot(x,xx)))
    }
  }
})


#' Edit control attribute
#'
#' @param x data of class STOMobs.
#' @param ... ... One or more settings value, see XXXX
#' @return  Data set of class STOMobs with changed control attributes
#' @export
edit_control<-function(x,...) {
  control<-attr(x,'control')
  types<-getSlots('STOMcontrol')

  args <- as.list(match.call()[-1][-1])
  gem<<-args
  vars<-names(args)

  for(i in seq(args)){
    var <- vars[i]
    arg <- args[[i]]
    fit <- var %in% slotNames(control)
    if(!fit) {
      cat("Warning - no match found for:",var,"\n")
    } else {
      type<-types[var]
      ok<-FALSE
      if (type=="character") ok<-is.character(eval(arg))
      else if (type=="expression") ok<-is.expression(eval(arg))
      else if (type=="list") ok<-is.list(eval(arg))
      else if (type=="vector") ok<-is.vector(eval(arg))
      else if (type=="logical") ok<-is.vector(eval(arg))
      cat(var,"type;",type,ok,'\n')
      if (ok) slot(control,var) <-eval(arg)
    }
  }
  attr(x,'control')<-control
  return(x)
}


#' Get control attribute
#'
#' @param x data of class STOMobs.
#' @return  Data of class STOMcontrol
#' @export
get_control<-function(x) {
  return(attr(x,'control'))
}


