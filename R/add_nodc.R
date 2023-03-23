#' Add NODC 10 digit numeric Id to predator or prey species where missing.
#' The NODC Taxonomic Code was based on 12 digit "intelligent" code numbers.
#' By "intelligent" code numbers we mean that information about taxonomy was built
#' into the codes through the use of 2-digit couplets to represent one or more levels
#' of the taxonomic hierarchy. For example, a species assigned a 10-digit code would
#' belong to the genus represented by the first 8 digits of the code.
#' The numerical hierarchy of the code numbers therefore reflected taxonomic hierarchy as
#' well, and his hierarchy is useful for grouping species with e.g. similar energy contents,
#' or within taxonomic model groups.
#' @title Add NODC Id where missing
#'
#' @param stom Stomach data set of class STOMobs, from e.g. \link{read_exchange_data}.
#' @param NODC_ID File Name including latin (or other) predator or prey name (column species and column NODC). File 'NODC_latin.csv' in the package /inst directory is used as default.
#' @param predator_or_prey Add missing NODC for "predator" or "prey".
#' @param stop_if_errror  Logical for stopping if NODC is not found.
#' @param delete_first Logical for deleting exiting predator or prey NODC.
#' @param verbose Show inserted NODC values.
#' @return Stomach data of class STOMobs with added \code{pred_nodc} or code{pred_nodc} variables.
#' @export
#' @examples \dontrun{add_NODC_ID(s)}
add_NODC_ID <- function(stom, NODC_ID, predator_or_prey = c("predator", "prey"), stop_if_errror = FALSE,delete_first=FALSE,verbose=FALSE) {

  dup_species<-dup_species<-NODC<-NODC<-pred_name<-pred_name<-prey_name<-prey_name<-species<-species<-NULL

  x<-as.data.frame(stom)
  predator_or_prey <- match.arg(predator_or_prey)

  if (delete_first) {
    if (predator_or_prey %in% c("pred")) x$pred_nodc<-NULL
    if (predator_or_prey %in% c("prey")) x$prey_nodc<-NULL
  }

  if (rlang::is_missing(NODC_ID)) {
    file <- system.file("extdata","NODC_latin.csv", package = "FishStomachs")
    b <- utils::read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
  } else b <- utils::read.csv(file = NODC_ID, stringsAsFactors = FALSE)

  dimb<-dim(b)[[1]]
  b<-unique(b)
  stopifnot("NODC list has not unique combinations of species and NODC values"=dimb==dim(b)[[1]])


  if (predator_or_prey %in%  c("prey")) {
    if (!("prey_nodc" %in% names(x))) x$prey_nodc <- as.numeric(NA)
    #   no_id <- subset(x, is.na(x$prey_nodc) & x$n_food >= 1)
    no_id <- subset(x, is.na(x$prey_nodc) )
    no_id <- sort(unique(no_id$prey_name))
    key <- match(no_id, b$species)
    if (any(is.na(key))) {
      cat("NODC for preys are not found:", paste(as.character(no_id[is.na(key)]),';',sep=''),'\n')
      if (stop_if_errror)
        stop("Data processing stopped\n")
    }
  } else if (predator_or_prey %in% c("predator")) {
    if (!("pred_nodc" %in% names(x))) x$pred_nodc <- as.numeric(NA)
    no_id <- subset(x, is.na(x$pred_nodc))
    no_id <- sort(unique(no_id$pred_name))
    key <- match(no_id, b$species)
    if (any(is.na(key))) {
      cat("NODC for predators are not found:",paste(as.character(no_id[is.na(key)]),';',sep=''),'\n')
      if (stop_if_errror)
        stop("Data processing stopped\n")
    }
  } else stop("Parameter predator_or_prey must be either 'prey' or 'predator\n' ")

  if (length(no_id) > 0) {
    if (verbose) cat(paste0(b$species[key], " -into- ", b$NODC[key], "\n"))

    # allocate missing prey NODC where exist
    if (predator_or_prey %in% c("prey")) {
      x <- dplyr::left_join(x = x, y = b, by = c("prey_name" = "species")) %>% dplyr::mutate(prey_name=factor(prey_name))
      #mis <- is.na(x$prey_nodc) & x$n_food >= 1
      mis <- is.na(x$prey_nodc)

      x[mis, 'prey_nodc'] <- x[mis, "NODC"]

    } else if (predator_or_prey %in% c("predator")) {
      x <- dplyr::left_join(x = x, y = b, by = c("pred_name" = "species")) %>%  dplyr::mutate(pred_name=factor(pred_name))
      mis <- is.na(x$pred_nodc)
      x[mis, 'pred_nodc'] <- x[mis, "NODC"]
    }
    mis[is.na(mis)] <- FALSE

    x$NODC <- NULL
  }
  if (predator_or_prey %in% c("predator")) x<-as_STOMobs(x,new_pred_var='pred_nodc') else x<-as_STOMobs(x,new_prey_var='prey_nodc')
  return(x)
}
