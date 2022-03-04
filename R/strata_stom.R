#' Aggregate stomach contents within strata (time, area and sub-area)
#'
#' Stomach contents data are aggregated (sum) within each combination of stratum_time, stratum_area and stratum_sub_area.
#'
#' The number of stomachs (n_tot) is dplyr::summarise within the strata.
#'
#' @title Aggregate stomach contents within strata (time, area and sub-area)
#' @param stom  stomach contents data of class STOMobs.
#' @return stomach contents data of class STOMstrata.
#' @examples \dontrun{b<-strata_aggegate(stom)}
strata_aggregate <- function(stom) {
  #  stom<-a
  pred<-NULL
  n_tot<-pred_name<-pred_size<-prey_name<-prey_size<-prey_w<-stratum_area<-stratum_sub_area<-stratum_time<-NULL
  b<-as.data.frame(stom)  %>% dplyr::group_by(stratum_time, stratum_area, stratum_sub_area,pred_name, pred_size, prey_name, prey_size) %>%
    dplyr::summarise(prey_w = sum(prey_w,na.rm=TRUE))
  nstom<-stom[['PRED']] %>% dplyr::group_by(stratum_time, stratum_area, stratum_sub_area,pred_name, pred_size) %>% dplyr::summarise(n_tot = sum(n_tot,na.rm=TRUE))
  b<-dplyr::left_join(b,nstom,by=c("stratum_time", "stratum_area", "stratum_sub_area", "pred_name", "pred_size")) %>% dplyr::ungroup()
  attr(b, "aggregated") <- 'strata sum of stomach contents'
  attr(b,"control")<-attr(stom,'control')
  return(b)
}


#' Table stomach contents within by strata (time, area and sub-area). Tables are returned or shown on screen.
#' @title Table stomach contents within by strata (time, area and sub-area)
#' @param s stomach contents data of class STOMstrata (produced by strata_aggegate)
#' @param use_criteria Use criteria in control to subset data
#' @param do_show Show the tables on the screen.
#' @param digits Number of digits in output.
#'
#' @return Tables of stomach contents.
#' @examples \dontrun{b<-strata_aggegate(stom)}

table_strata_prey_preyl <- function(s, use_criteria=FALSE,do_show = TRUE,digits = 1) {
  if (use_criteria) s<-make_criteria_strata_set(s)
  s<-s %>% dplyr::mutate_if(is.factor, as.character)
  tabs <- by(s, list( s$pred_name,s$stratum_area,s$stratum_time,s$pred_size ), function(x) {
    ft <- NULL
    if (!is.null(x)) {
      ft <- tapply(x$prey_w, list(x$prey_size, x$prey_name), sum)
      ft_row <- rowSums(ft, na.rm = TRUE)
      ft <- cbind(ft, All = ft_row)
      ft_col <- colSums(ft, na.rm = TRUE)
      ft <- rbind(ft, All = ft_col)
      if (do_show) {
        cat(paste("\n\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n",
                  x[1, "stratum_time"], x[1, "stratum_area"], x[1, "pred_name"], x[1, "pred_size"], "\n"))
        print(round(ft, digits))
      }
    }
    return(ft)
  })
  invisible(tabs)
}


##############
table_strata_prey <- function(stom, digits = 1, return_tab = FALSE, do_print = TRUE) {
  tabs <- by(stom, list(stom$stratum_time, stom$stratum_area, stom$pred_name), function(x) {
    ft <- NULL
    if (!is.null(x)) {
      ft <- cbind(tapply(x$prey_w, list(x$pred_size, x$prey_name), sum), All = tapply(x$prey_w, list(x$pred_size), sum))
      if (do_print) {
        cat(paste("\n\n", x[1, "stratum_time"], x[1, "stratum_area"], x[1, "pred_name"], "\n"))
        print(round(ft, digits))
      }
    }
    return(ft)
  })
  if (return_tab)
    return(tabs) else return(NULL)
}

############
