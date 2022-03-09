
#' Redistribute partly identified preys to the distibution of the identified prey species from the same predator and predator size
#' within the given area and time strata.
#'
#' The function allocate partly identified preys into identified preys. A group of partly identified could for example be Clupeidae (herring like fish)
#' which should be distributed according to the of observed proportion of identified preys within that group (e.g. herring and sprat).
#' The parameter \code{from_to_species}
#' specify which identified prey items that is included in a group of not fully identified prey items. \code{from_to_species} can include more than one group of
#' partly identified groups, e.g. Clupeidae and Gadidae.  See function the documentation for \code{\link{make_from_to_species}}.
#'
#' Input data must include the variables \code{stratum_time}, \code{stratum_sub_area} and \code{stratum_area}.
#' These variable are normally added by the the function \code{\link{add_strata}}.
#'
#' The parameters \code{dist_time}, \code{dist_area} and \code{dist_pred_size} define the groups to be used for producing a prey allocation keys.
#' The variables can either include a variable from the input data (e.g. dist_area could be set to rectangle) or be a constant, e.g. 'all'.  Each combination of
#' a distribution key of identified preys within prey group is made for for each combination of \code{dist_time}, \code{dist_area} and \code{dist_pred_size}
#' and used to redistribute the the partly identified prey for each of the combinations.
#'
#' As an example:
#' The call redist_unidentified_prey_sp(b,dist_time=stratum_time,dist_area=stratum_sub_area,dist_pred_size=pred_size)
#' uses the variable stratum_time and stratum_sub_area defined in the input set, to define groups
#' to redistribution of partly identified preys. That means that only observations of identified preys within the each sub_area
#' is used for redistribution, with the risk that there are no or few observation of fully identified preys, such that the distribution
#' key becomes inadequate.
#'
#' The call redist_unidentified_prey_sp(v,dist_time=stratum_time,dist_area=stratum_area,dist_pred_size=pred_size)
#' does the same, but here is the groups defined from the larger stratum_area instead of strataum_sub_area.
#' This will give more observations to make the distribution key, but will also use observations from a larger area.
#'
#' The call redist_unidentified_prey_sp(v,dist_time=year,dist_area='ALL',dist_pred_size=pred_size)
#' will use all observations (all available quarterly observations) within a year and all areas to make the distribution key
#' used for redistribution.
#'
#' @title Redistribute unidentified prey species
#' @param s  Stomach contents data of class STOMobs with variables \code{stratum_time}, \code{stratum_sub_area} and \code{stratum_area}.
#' @param dist_time Stratum used for time in constructing proportion of species within a prey group of not fully identified prey items. The variable should be made from variable names in s, or constants.
#' @param dist_area Stratum used for area in constructing proportion of species within a prey group of not fully identified prey items. The variable should be made from variable names in s, or constants.
#' @param dist_pred_size Stratum used for predator size in constructing proportion of species within a prey group of not fully identified prey items. The variable should be made from variable names in s, or constants.
#' @param from_to_species Allocation key for not fully identified prey species into identified species. See function \code{\link{make_from_to_species}}.
#' @param do_only Do only redistribution for the selected steps.
#' @param selected_pred Predators for which redistribution is done. All predators are included,if \code{selected_pred value} is missing.
#' @param by_prey_size Do the allocation by prey and prey size (\code{by_prey_size=TRUE}) or just by prey (\code{by_prey_size=FALSE}).
#' @param verbose Logical, show details
#' @param remains_to_other Convert not fully identified prey species to prey 'other' where no species allocation key can be found.
#' @return Stomach contents data of class STOMobs.
#' @export
#' @examples \dontrun{x=2}
#'
redist_unidentified_prey_sp <- function(s, dist_time = stratum_time, dist_area = stratum_area, dist_pred_size = pred_size, from_to_species, do_only, selected_pred, remains_to_other = TRUE,
    by_prey_size = TRUE,verbose=FALSE) {
    # test s=b; from_to_species=from_to;remains_to_other = FALSE; selected_pred<-sort(unique(s[['PRED']]$pred_name)); do_only=c(1,2); by_prey_size<-FALSE
  fish_id<-from_prey_name<-pred_name<-pred_size<-prey_name<-prey_w<-proportion<-sample_id<-stratum_area<-stratum_time<-to_species<-NULL
  pred_size_class<-prey_size<-NULL

    if (!("data.frame" %in% class(from_to_species)))
        stop("from_to_species does not have the right format, see function make_from_to_species()")

    s_org <- s

    if (missing(selected_pred))
        selected_pred <- sort(unique(s[["PRED"]]$pred_name))
    control <- attr(s, "control")
    mis_prey_len <- paste0(control@mis_l, "-", control@mis_l)
    other <- control@other
    mis_prey_size_class <- control@mis_size_class

    PRED_names <- colnames(s[["PRED"]])
    PREY_names <- colnames(s[["PREY"]])
    to_be_kept <- dplyr::select(s[["PREY"]], sample_id, fish_id) %>%
        dplyr::distinct()

    s <- as.data.frame(s)

    dist_time <- rlang::enquo(dist_time)
    dist_area <- rlang::enquo(dist_area)
    dist_pred_size <- rlang::enquo(dist_pred_size)

    # add strata definitions to s
    s <- dplyr::mutate(s, dist_time = !!dist_time, dist_area = !!dist_area, dist_pred_size = !!dist_pred_size)
    # test s<-dplyr::mutate(s,dist_time=stratum_time, dist_area=stratum_area, dist_pred_size=pred_size)

    unchanged <- dplyr::filter(s, !(pred_name %in% selected_pred))
    s <- dplyr::filter(s, pred_name %in% selected_pred)

    sptof_all <- from_to_species %>%
        dplyr::transmute(order = order, from_prey_name = factor(from_species, levels = levels(s$prey_name)), prey_name = factor(to_species, levels = levels(s$prey_name))) %>%
        dplyr::filter(!(is.na(from_prey_name) | is.na(prey_name)))

    iter <- sort(unique(from_to_species$order))
    if (!(missing(do_only)))
        iter <- intersect(iter, do_only)

    for (i in iter) {
        # test i<-1
        from_species <- unique(dplyr::filter(sptof_all, order == i)$from_prey_name)
        sptof <- dplyr::filter(sptof_all, order == i) %>%
            dplyr::select(-order)

        s <- dplyr::mutate(s, row = 1:nrow(s), from_prey_name = NULL)

        remains <- dplyr::filter(s, !(prey_name %in% from_species))
        # old dist <- dplyr::filter(s, prey_name %in% from_species) %>% dplyr::mutate(from_prey_name = prey_name, prey_name=NULL, prey_size = NULL, prey_size_class=NULL)
        dist <- dplyr::filter(s, prey_name %in% from_species) %>%
            dplyr::mutate(from_prey_name = prey_name, prey_name = NULL)
        if (!by_prey_size)
            dist <- dplyr::mutate(dist, prey_size = NULL, prey_size_class = NULL)

        to_be_dist <- dim(dist)[[1]]


        dist_key <- dplyr::filter(s, prey_name %in% sptof$prey_name)
        dist_key <- dplyr::left_join(dist_key, sptof, by = "prey_name")

        strata_rel_dist <- function(x) {
            a1 <- x %>%
                dplyr::group_by(dist_time, dist_area, pred_name, dist_pred_size, from_prey_name, prey_name, prey_size, prey_size_class) %>%
                dplyr::summarise(prey_w = sum(prey_w, na.rm = TRUE))
            if (by_prey_size)
                a1 <- a1 %>%
                  dplyr::group_by(dist_time, dist_area, pred_name, dist_pred_size, from_prey_name, prey_size, prey_size_class) %>%
                  dplyr::mutate(proportion = prey_w/sum(prey_w, na.rm = TRUE)) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(-prey_w) %>%
                  dplyr::filter(!is.na(proportion))
            if (!by_prey_size)
                a1 <- a1 %>%
                  dplyr::group_by(dist_time, dist_area, pred_name, dist_pred_size, from_prey_name) %>%
                  dplyr::mutate(proportion = prey_w/sum(prey_w, na.rm = TRUE)) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(-prey_w) %>%
                  dplyr::filter(!is.na(proportion))
            return(a1)
        }
        rel_key <- strata_rel_dist(dist_key)
        if (by_prey_size)
            found <- dplyr::inner_join(dist, rel_key, by = c("pred_name", "dist_time", "dist_area", "dist_pred_size", "from_prey_name", "prey_size_class", "prey_size")) %>%
                dplyr::mutate(prey_w = prey_w * proportion, proportion = NULL, prey_n = NA, prey_l_mean = NA, type = "found")

        if (!by_prey_size)
            found <- dplyr::inner_join(dist, rel_key, by = c("pred_name", "dist_time", "dist_area", "dist_pred_size", "from_prey_name")) %>%
                dplyr::mutate(prey_w = prey_w * proportion, proportion = NULL, prey_n = NA, prey_l_mean = NA, type = "found")

        found_rows <- unique(found$row)
        l_found_rows <- length(found_rows)
        not_found <- dplyr::filter(dist, !(row %in% found_rows)) %>%
            dplyr::mutate(prey_name = from_prey_name, prey_size = factor(mis_prey_len, levels = levels(s$prey_size)), prey_size_class = mis_prey_size_class)

        if (remains_to_other) {
          not_found <- not_found %>%
            dplyr::mutate(prey_name = factor(other, levels = levels(s$prey_name)), prey_size = factor(mis_prey_len, levels = levels(s$prey_size)), prey_size_class = mis_prey_size_class,
                          prey_n = 0, prey_l_mean = NA)
        }

        s <- dplyr::select(dplyr::bind_rows(remains, found, not_found), -row)
        if (verbose) cat("Redist. ", as.character(from_species), " Records to be dis.:", to_be_dist)
        if (to_be_dist >0) {
          if (verbose) cat(",  found:", l_found_rows, "  ")
          if ( remains_to_other) if (verbose) cat("as other:", dim(not_found)[[1]]) else {if (verbose) cat("un-allocated:", dim(not_found)[[1]])}
        }
        if (verbose) cat("\n")
    }

    s <- dplyr::bind_rows(s, unchanged)

    s_org[["PRED"]] <- dplyr::select(s, dplyr::all_of(PRED_names)) %>%
        dplyr::distinct()

    s <- dplyr::left_join(to_be_kept, s, by = c("sample_id", "fish_id"))  # to delete empty stomachs

    s_org[["PREY"]] <- dplyr::select(s, dplyr::all_of(PREY_names))

    attr(s_org, all_stom_attributes()["mis_prey"]) <- TRUE
    return(s_org)
}


#'  Redistribute preys with no length information
#'
#'  Redistribute missing length observations using the observed length distribution of the species '
#'  from the same predator and predator size class within the given area and time strata. Only prey species '
#'  specified by control@sel_preys are redistributed.'
#' @title Redistribute unidentified prey lengths
#' @param s  Stomach contents data of class STOMobs.
#' @param dist_time Stratum used for time in constructing proportion of species within a prey group of not fully identified prey items. The variable should be made from variable names in s, or constants.
#' @param dist_area Stratum used for area in constructing proportion of species within a prey group of not fully identified prey items. The variable should be made from variable names in s, or constants.
#' @param dist_pred_size Stratum used for predator size in constructing proportion of species within a prey group of not fully identified prey items. The variable should be made from variable names in s, or constants.
#' @param selected_pred Predators for which redistribution is done. All predators included in s is selected if selected_pred value is missing.
#' @param remains_to_other Convert not fully identified prey species to prey 'other' where no species allocation key cannot be found.
#' @param others_to_other Convert not species not included in \code{selected_pred} to prey 'other'.
#' @param verbose, Logical, show delatils
#' @export
#' @return s Stomach contents data of class STOMobs.
redist_unidentified_prey_lengths <- function(s, dist_time = stratum_time, dist_area = stratum_area, dist_pred_size = pred_size, selected_pred, remains_to_other = TRUE,others_to_other=TRUE,verbose=FALSE) {
    # test s=b; from_to_species=from_to;remains_to_other = FALSE; selected_pred<-sort(unique(s[['PRED']]$pred_name))
    fish_id<-pred_name<-pred_size<-prey_name<-prey_size<-prey_size_class<-prey_w<-proportion<-sample_id<-stratum_area<-stratum_time<-NULL

    s_org <- s
    if (missing(selected_pred))
        selected_pred <- sort(unique(s[["PRED"]]$pred_name))
    control <- attr(s, "control")
    mis_prey_len <- paste0(control@mis_l, "-", control@mis_l)
    other <- control@other
    mis_prey_size_class <- control@mis_size_class
    sel_prey <- control@sel_preys

    PRED_names <- colnames(s[["PRED"]])
    PREY_names <- colnames(s[["PREY"]])
    to_be_kept <- dplyr::select(s[["PREY"]], sample_id, fish_id) %>%
        dplyr::distinct()

    s <- as.data.frame(s)

    dist_time <- rlang::enquo(dist_time)
    dist_area <- rlang::enquo(dist_area)
    dist_pred_size <- rlang::enquo(dist_pred_size)

    if (verbose) cat("Prey size will be allocated for species:", paste(sel_prey, collate = ", "), "\n")
    # add strata definitions to s
    s <- dplyr::mutate(s, dist_time = !!dist_time, dist_area = !!dist_area, dist_pred_size = !!dist_pred_size)
    # test s<-dplyr::mutate(s,dist_time=stratum_time, dist_area=stratum_area, dist_pred_size=pred_size)

    unchanged <- dplyr::filter(s, !(pred_name %in% selected_pred))
    s <- dplyr::filter(s, pred_name %in% selected_pred) %>%
        dplyr::mutate(row = 1:nrow(s))

    remains <- dplyr::filter(s, !((prey_name %in% sel_prey) & (prey_size == mis_prey_len)))

    dist <- dplyr::filter(s, (prey_name %in% sel_prey) & (prey_size == mis_prey_len)) %>%
        dplyr::select(-prey_size, -prey_size_class)
    to_be_dist <- dim(dist)[[1]]
    dist_key <- dplyr::filter(s, (prey_name %in% sel_prey) & (prey_size != mis_prey_len))


    strata_rel_dist <- function(x) {
        a1 <- x %>%
            dplyr::group_by(dist_time, dist_area, pred_name, dist_pred_size, prey_name, prey_size, prey_size_class) %>%
            dplyr::summarise(prey_w = sum(prey_w, na.rm = TRUE)) %>%
            dplyr::group_by(dist_time, dist_area, pred_name, dist_pred_size, prey_name) %>%
            dplyr::mutate(proportion = prey_w/sum(prey_w, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-prey_w) %>%
            dplyr::filter(!is.na(proportion))
        return(a1)
    }
    rel_key <- strata_rel_dist(dist_key)


    found <- dplyr::inner_join(dist, rel_key, by = c("pred_name", "prey_name", "dist_time", "dist_area", "dist_pred_size")) %>%
        dplyr::mutate(prey_w = prey_w * proportion, proportion = NULL, prey_n = 0)

    found_rows <- unique(found$row)
    l_found_rows <- length(found_rows)

    not_found <- dplyr::filter(dist, !(row %in% found_rows)) %>%
        dplyr::mutate(prey_size = factor(mis_prey_len, levels = levels(s$prey_size)), prey_size_class = mis_prey_size_class)

    if (remains_to_other) {
        not_found <- not_found %>%
            dplyr::mutate(prey_name = factor(other, levels = levels(s$prey_name)), prey_size = factor(mis_prey_len, levels = levels(s$prey_size)), prey_size_class = mis_prey_size_class,
                prey_n = 0)
    }

    if (others_to_other) {
      remains<-bind_rows(
       remains %>% filter(!(prey_name %in% c(sel_prey,other)))  %>%
        dplyr::mutate(prey_name = factor(other, levels = levels(s$prey_name)),
        prey_size = factor(mis_prey_len, levels = levels(s$prey_size)),
        prey_size_class = mis_prey_size_class,prey_n = 0),
       remains %>% filter( (prey_name %in% c(sel_prey,other)))
      )
    }

    s <- dplyr::select(dplyr::bind_rows(remains, found, not_found), -row)
    if (verbose) cat("Records to be distributed:", to_be_dist, ",  found:", l_found_rows, "  Un-allocated:", dim(not_found)[[1]], "\n")


    s <- dplyr::bind_rows(s, unchanged)

    s_org[["PRED"]] <- dplyr::select(s, dplyr::all_of(PRED_names)) %>%
        dplyr::distinct()
    s <- dplyr::left_join(to_be_kept, s, by = c("sample_id", "fish_id"))  # to delete empty stomachs
    s_org[["PREY"]] <- dplyr::select(s, dplyr::all_of(PREY_names))

    attr(s_org, all_stom_attributes()["mis_size"]) <- TRUE
    return(s_org)
}


#' Read csv-file with allocation keys for partly identified prey species.
#'
#' @param inp_file Input file name. The file must include the variable names order, from_species and to_species.
#'
#' @return Data frame with names of partly identified preys and their allocation into identified preys
#' @export
#' @examples \dontrun{from_to<-make_from_to_species();}
#'
make_from_to_species<-function(inp_file='from_to_species.csv') {
  from_species<-from_species<-to_species<-to_species<-NULL

  ft<-dplyr::as_tibble(read.csv(file=inp_file,stringsAsFactors=FALSE))
  if (!("order" %in% colnames(ft) & "from_species" %in% colnames(ft) & "to_species" %in% colnames(ft))) stop("Function stop: input file does not include the variable names 'order', 'from_species' and 'to_species'\n")

  ft<-  dplyr::distinct(ft) %>% arrange(order,from_species,to_species) %>% dplyr::select(order,from_species,to_species)
  return(ft)
}


