#' Put size classes on predators
#'
#' This function adds a size group to each predator based on the predator length (pred_l) if such exist,
#' or based on the midpoint of the size range (pred_ll - pred_lu). Both approaches use an input definition of size groups,
#' typically made by the function \code{make_length_classes}.
#' The mean length of the predator (pred_l_mean) is added either copied from the variable pred_l or from the midpoint of the size range (pred_ll - pred_lu)
#' The size clases are species independent if \code{len_classes} includes Species==ALL. .
#' If length classes is given by predator in \code{len_classes}, species specific length classes will be added.
#'
#' @title Put size classes on predator from observed length.
#' @param stom  stomach contents data of class STOMobs.
#' @param len_classes Data frame with size class definitions. See \code{make_length_classes}. Size class will be added just for the species included in \code{len_classes}. If \code{length_classes} include sprecies='ALL' all species will get size class.
#' @export
#'
#' @return stomach contents data of class STOMobs wit added size class for predators.
put_size_class_on_predator <- function(stom, len_classes) {
    # TEST stom=s; len_classes=ll;
  group<-l1<-l2<-no<-pred_l<-pred_l_mean<-pred_ll<-pred_lu<-pred_luu<-pred_name<-pred_size<-Species<-NULL
    control <- attr(stom, "control")
    mis_l <- as.integer(control@mis_l)

    species <- unique(stom[["PRED"]]$pred_name)
    if (any("ALL" %in% len_classes$Species)) {
        len_classes <- dplyr::filter(len_classes, Species == "ALL")
        len_classes <- do.call(dplyr::bind_rows, lapply(species, function(x) {
            len_classes$Species <- x
            return(len_classes)
        }))
        cat("The same length classes wil be applied for all predators\n")
    } else {
        species <- species[species %in% unique(len_classes$Species)]
        cat("length classes wil be applied for the predators :", paste(species, collapse = ", "), "\n")

    }

    st <- stom[["PRED"]]

    len_classes <- dplyr::filter(len_classes, Species %in% species) %>%
        dplyr::mutate(Species = factor(Species, levels = levels(st$pred_name)))

    excl <- dplyr::filter(st, !(pred_name %in% species))

    incl <- dplyr::filter(st, pred_name %in% species) %>%
        dplyr::mutate(pred_luu = as.integer(dplyr::if_else(pred_lu == mis_l, as.integer(round(pred_ll * 1.1)), pred_lu))) %>%
        dplyr::mutate(pred_l_mean = dplyr::if_else(pred_l == mis_l | is.na(pred_l), as.integer(round((pred_ll + pred_luu)/2)), as.integer(pred_l)), pred_luu = NULL) %>%
        dplyr::mutate(row = dplyr::row_number())
    as.data.frame(incl)

    # do species with length classes
    incl2 <- dplyr::left_join(x = incl, y = len_classes, by = c(pred_name = "Species", year = "year", quarter = "quarter")) %>%
        dplyr::filter((pred_l_mean >= l1 & pred_l_mean <= l2)) %>%
        dplyr::mutate(pred_ll = l1, pred_lu = l2, l1 = NULL, l2 = NULL) %>%
        dplyr::rename(pred_size_class = no, pred_size = group)

    dif <- setdiff(incl$row, incl2$row)
    if (length(dif) > 0) {
        tst <- dplyr::filter(incl, row %in% dif)
        cat(paste("Size classes were not found for ", dim(tst)[[1]], " records\n"))
        print(tst)
        # print(dplyr::select(tst, -prey_ll, -prey_ll, -prey_w, -prey_n, -starts_with('n_'), -weight_method, -dataset, -record_type, -row))
        cat("These records have been deleted\n")
    }
    incl2 <- dplyr::select(incl2, -row)


    stom[["PRED"]] <- dplyr::bind_rows(incl2, excl) %>%
        dplyr::mutate(pred_size = fct_na_value_to_level(pred_size))
    attr(stom, all_stom_attributes()["pred_size"]) <- TRUE
    return(stom)
}



#' Put size classes on preys
#'
#' This function adds a size group to each prey item based on the prey length (prey_l) if such exist,
#' or based on the midpoint of the size range (prey_ll - prey_lu). Both approaches use an input definition of size groups,
#' typically made by the function \code{make_length_classes}.
#' The mean length of the predator (XXXXX) is added either copied from the variable prey_l or from the midpoint of the size range (prey_ll - prey_lu)
#' The size clases are species independent if \code{len_classes} includes Species=ALL. .
#' If length classes is given by predator in \code{len_classes}, species specific length classes will be added.
#'
#' @title Put size classes on preys from observed length.
#' @param stom  stomach contents data of class STOMobs.
#' @param len_classes Data frame with size class definitions. See \code{make_length_classes}. Size class will be added just for the species included in \code{len_classes}. If \code{length_classes} include sprecies='ALL' all species will get size class.
#' @param too_small_to_other Convert small prey items into other food. Preys smaller than defined by control@min_prey_length are renamed into other food with unknown size if \code{too_small_to_other} is TRUE.
#'
#' @export
#' @return stomach contents data of class STOMobs with added size class for predators.
put_size_class_on_prey <- function(stom, len_classes, too_small_to_other = TRUE) {
    # stom<-s; len_classes=ll ; too_small_to_other=TRUE

    group<-l1<-l2<-no<-prey_l_mean<-prey_ll<-prey_lu<-prey_name<-prey_size<-quarter<-Species<-NULL
    fish_id<-fish_id<-sample_id<-sample_id<-year<-year<-NULL

    control <- attr(stom, "control")
    mis_l <- as.integer(control@mis_l)
    other <- control@other  # id for 'other food'
    mis_prey_size_class <- control@mis_size_class
    min_prey_length <- control@min_prey_length
    mis_prey_size <- paste(mis_l, mis_l, sep = "-")

    if ("prey_size_class" %in% names(stom[['PREY']])) stom[['PREY']]$prey_size_class<-NULL
    if ("prey_size" %in% names(stom[['PREY']])) stom[['PREY']]$prey_size<-NULL
    species <- unique(stom[["PREY"]]$prey_name)
    if (any("ALL" %in% len_classes$Species)) {
        len_classes <- dplyr::filter(len_classes, Species == "ALL")
        len_classes <- do.call(dplyr::bind_rows, lapply(species, function(x) {
            len_classes$Species <- x
            return(len_classes)
        }))
        cat("The same length classes wil be applied for all preys.\n")
    } else {
        species <- species[species %in% unique(len_classes$Species)]
        cat("length classes wil be applied for the preys :", paste(species, collapse = ", "), "\n")
    }

    preys <- stom[["PREY"]] %>%
        dplyr::mutate(prey_name = as.character(prey_name))
    # summary(dplyr::filter(preys,prey_name=='other'))
    if (too_small_to_other) {

        crit <- preys$prey_ll < min_prey_length
        crit[is.na(crit)] <- FALSE

        # xtabs(~prey_name+prey_ll,data=subset(preys,crit)) xtabs(~prey_name+prey_ll,data=subset(preys,!crit))

        preys[crit, "prey_name"] <- other
        preys[crit, "prey_ll"] <- mis_l
        preys[crit, "prey_lu"] <- mis_l
        preys[crit, "prey_l"] <- NA
        preys[crit, "prey_n"] <- NA
        if ("prey_nodc" %in% colnames(preys))
            preys[crit, "prey_nodc"] <- -9
    }
    # summary(dplyr::filter(preys,prey_name=='other'))

    st <- dplyr::left_join(preys, dplyr::select(stom[["PRED"]], sample_id, fish_id, year, quarter), by = c("sample_id", "fish_id"))
    st[is.na(st$prey_l), "prey_l"] <- mis_l
    st[is.na(st$prey_ll), "prey_ll"] <- mis_l
    st[is.na(st$prey_lu), "prey_lu"] <- mis_l

    excl <- dplyr::filter(st, !(prey_name %in% species))  # not dplyr::selected species

    incl <- dplyr::filter(st, prey_name %in% species) %>%
        dplyr::mutate(prey_l_mean = dplyr::if_else(prey_ll == mis_l | prey_lu == mis_l, mis_l, as.integer(round((prey_ll + prey_lu)/2))))

    excl2 <- dplyr::filter(incl, prey_l_mean == mis_l)  # insufficient length information
    if (dim(excl2)[[1]] > 0) {
        # excl2 <- excl2 %>% dplyr::mutate( prey_size = paste(prey_ll, prey_lu, sep = '-'), prey_size_class = mis_prey_size_class)
        excl <- dplyr::bind_rows(excl, excl2)
    }

    incl <- dplyr::filter(incl, prey_l_mean != mis_l) %>%
        dplyr::mutate(row = dplyr::row_number())
    # sum(st$prey_w,na.rm=TRUE)- (sum(incl$prey_w,na.rm=TRUE)+sum(excl$prey_w,na.rm=TRUE))


    # do species with length classes
    len_classes <- dplyr::mutate(len_classes, Species = as.character(Species))
    incl2 <- dplyr::left_join(x = incl, y = len_classes, by = c(prey_name = "Species", year = "year", quarter = "quarter")) %>%
        dplyr::filter((prey_l_mean >= l1 & prey_l_mean <= l2)) %>%
        dplyr::mutate(prey_ll = l1, prey_lu = l2, l1 = NULL, l2 = NULL) %>%
        dplyr::rename(prey_size_class = no, prey_size = group)


    dif <- setdiff(incl$row, incl2$row)
    if (length(dif) > 0) {
        tst <- dplyr::filter(incl, row %in% dif)
        print(paste("Size classes were not found for ", dim(tst)[[1]], " records"))
        print(dplyr::select(tst, -row))
        print("Preys from these records were renamed to 'other food'")
        excl2 <- tst %>%
            dplyr::mutate(row = NULL, prey_name = other, prey_ll = mis_l, prey_lu = mis_l)
        # sum(excl2$prey_w,na.rm=TRUE)
        excl <- dplyr::bind_rows(excl, excl2)
    }

    excl <- excl %>%
        dplyr::mutate(prey_size = mis_prey_size, prey_size_class = mis_prey_size_class)

    incl2 <- dplyr::mutate(incl2, row = NULL)
    # sum(st$prey_w,na.rm=TRUE)- (sum(incl2$prey_w,na.rm=TRUE)+sum(excl$prey_w,na.rm=TRUE))



    stom[["PREY"]] <- dplyr::bind_rows(incl2, excl) %>%
        dplyr::mutate(prey_name = fct_na_value_to_level(prey_name), prey_size = fct_na_value_to_level(prey_size), year = NULL, quarter = NULL)

    # sum(st$prey_w,na.rm=TRUE)-sum(stom[['PREY']]$prey_w,na.rm=TRUE) summary(dplyr::filter(stom[['PREY']],prey_name=='other'))
    attr(stom, all_stom_attributes()["prey_size"]) <- TRUE

    return(stom)
}

