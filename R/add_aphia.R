#' Add WoRMS Aphia Id where missing
#' @title Add WoRMS Aphia Id to predator or prey species where missing
#' @param stom Stomach data set of class STOMobs, from e.g. \link{read_exchange_data}).
#' @param Worms_Aphia_ID File Name including WoRMS Aphia ID by latin (or other) predator or prey name.file 'WoRM_AphiaID.csv' in the package /inst directory is used as default.
#' @param predator_or_prey Add missing Aphia ID for predator | prey.
#' @param stop_if_errror Logical for stopping if AphiaID is not found for all.
#' @param delete_first Delete all existing WoRMS Aphia Id before adding new
#' @param verbose Show information on progress
#' @param match_item Variable name in WoRM_AphiaID.csv ("item" | "scientific') used to assign WoRMS Aphia ID.
#' @return Stomach data on on internal format.
#' @export
#' @examples \dontrun{add_Aphia_ID(X)}
add_Aphia_ID <- function(stom, Worms_Aphia_ID, predator_or_prey = c("predator", "prey")[2], stop_if_errror = FALSE,delete_first=FALSE,verbose=FALSE,match_item=c('item','scientific')[2]) {

  item<-pred_name<-prey_name<-scientific<-WoRMS_AphiaID<-NULL
  x<-as.data.frame(stom)

    if (delete_first) {
        if (predator_or_prey %in% c("pred")) x$pred_aphia<-NULL
        if (predator_or_prey %in% c("prey")) x$prey_aphia<-NULL
    }

    if (rlang::is_missing(Worms_Aphia_ID)) {
        file <- system.file("WoRM_AphiaID.csv", package = "FishStomachs")
        b <- utils::read.csv(file, strip.white = TRUE, stringsAsFactors = FALSE)
    } else b <- utils::read.csv(file = Worms_Aphia_ID, stringsAsFactors = FALSE)

    if (match_item=='item') {
      b <- subset(b, select = c(item, WoRMS_AphiaID)) %>% dplyr::rename(species=item) %>% unique()
    } else  if (match_item=='scientific') {
       b <- subset(b, select = c(scientific, WoRMS_AphiaID)) %>% dplyr::rename(species=scientific) %>% unique()
    }


    dimb<-dim(b)[[1]]
#    stopifnot("NODC list has not unique combinations of species and NODC values"=dimb==dim(b)[[1]])
#    stopifnot("NODC list has not unique NODC values"=dimb==length(unique(b$NODC)))
#    stopifnot("NODC list has not unique species values"=dimb==length(unique(b$species)))


    if (predator_or_prey %in%  c("prey")) {
        if (!("prey_aphia" %in% names(x))) x$prey_aphia <- as.numeric(NA)
        no_id <- subset(x, is.na(x$prey_aphia) & x$n_food >= 1)
        no_id <- sort(unique(no_id$prey_name))
        key <- match(no_id, b$species)
        if (any(is.na(key))) {
            cat("aphia ID for preys are not found:\n")
            cat(paste(b[no_id[is.na(key)],'species'],'\n'))
            if (stop_if_errror)
                stop("Data processing stopped\n")
        }
    } else if (predator_or_prey %in% c("predator")) {
        if (!("pred_aphia" %in% names(x))) x$pred_aphia <- as.numeric(NA)
        no_id <- subset(x, is.na(x$pred_aphia))
        no_id <- sort(unique(no_id$pred_name))
        key <- match(no_id, b$species)
        if (any(is.na(key))) {
            cat("aphia for predators are not found:", no_id[is.na(key)])
            if (stop_if_errror)
                stop("Data processing stopped\n")
        }
    } else stop("Parameter predator_or_prey must be either 'prey' or 'predator\n' ")

    if (length(no_id) > 0) {
        if (verbose) cat(paste0(b$species[key], " -into- ", b$aphia[key], "\n"))

        # allocate missing prey aphia where exist
        if (predator_or_prey %in% c("prey")) {
            x <- dplyr::left_join(x = x, y = b, by = c("prey_name" = "species")) %>% mutate(prey_name=factor(prey_name))
            mis <- is.na(x$prey_aphia) & x$n_food >= 1
            x[mis, 'prey_aphia'] <- x[mis, "WoRMS_AphiaID"]

        } else if (predator_or_prey %in% c("predator")) {
            x <- dplyr::left_join(x = x, y = b, by = c("pred_name" = "species")) %>%  mutate(pred_name=factor(pred_name))
            mis <- is.na(x$pred_aphia)
            x[mis, 'pred_aphia'] <- x[mis, "WoRMS_AphiaID"]
        }
        mis[is.na(mis)] <- FALSE

        x$WoRMS_AphiaID <- NULL
    }
    if (predator_or_prey %in% c("predator")) x<-as_STOMobs(x,new_pred_var='pred_aphia') else x<-as_STOMobs(x,new_prey_var='prey_aphia')
    return(x)
}

