
### sum stomach contents by pool
pool_sum <- function(stom) {
  n_tot<-pool<-pred_name<-pred_size<-prey_name<-prey_size<-prey_w<-stratum_area<-stratum_time<-NULL
    b <- stom %>%
        dplyr::group_by(stratum_time, stratum_area, pool, n_tot, pred_name, pred_size, prey_name, prey_size) %>%
        dplyr::summarise(prey_w = sum(prey_w, na.rm = TRUE))
    attr(b, "aggregated") <- "pool sum of stomach contents"
    return(dplyr::ungroup(b))
}



### average stomach contents by strata
strata_average <- function(stom, nstom) {
  n_tot<-pred_name<-pred_size<-prey_name<-prey_size<-prey_w<-stratum_area<-stratum_time<-NULL
    a1 <- stom %>%
        dplyr::group_by(stratum_time, stratum_area, pred_name, pred_size, prey_name, prey_size) %>%
        dplyr::summarise(prey_w = sum(prey_w, na.rm = TRUE))
    a2 <- nstom %>%
        dplyr::group_by(stratum_time, stratum_area, pred_name, pred_size) %>%
        dplyr::summarise(n_tot = sum(n_tot))
    b <- dplyr::left_join(a1, a2, by = intersect(names(a1), names(a2))) %>%
        dplyr::mutate(prey_w = prey_w/n_tot)
    attr(b, "aggregated") <- "strata average of stomach contents"
    return(dplyr::ungroup(b))
}


### average stomach contents by pool and strata
pool_average <- function(stom, nstom) {
  n_tot<-n_tot<-pool<-pool<-pred_name<-pred_name<-pred_size<-pred_size<-prey_name<-prey_name<-prey_size<-prey_size<-prey_w<-prey_w<-stratum_area<-stratum_area<-stratum_time<-stratum_time<-NULL
    a1 <- stom %>%
        dplyr::group_by(stratum_time, stratum_area, pool, pred_name, pred_size, prey_name, prey_size) %>%
        dplyr::summarise(prey_w = sum(prey_w, na.rm = TRUE))
    a2 <- nstom %>%
        dplyr::group_by(stratum_time, stratum_area, pool, pred_name, pred_size) %>%
        dplyr::summarise(n_tot = sum(n_tot))
    b <- dplyr::left_join(a1, a2, by = intersect(names(a1), names(a2))) %>%
        dplyr::mutate(prey_w = prey_w/n_tot)
    attr(b, "aggregated") <- "strata average of stomach contents"
    return(dplyr::ungroup(b))
}


### relative stomach contents by stratum
strata_rel <- function(s) {
  pred_name<-pred_size<-prey_name<-prey_size<-prey_w<-stratum_area<-stratum_time<-sum_prey_w<-NULL
    a1 <- s %>%
        dplyr::select(stratum_time, stratum_area, pred_name, pred_size, prey_name, prey_size, prey_w)
    a2 <- a1 %>%
        dplyr::group_by(stratum_time, stratum_area, pred_name, pred_size) %>%
        dplyr::summarise(sum_prey_w = sum(prey_w, na.rm = TRUE))
    b <- dplyr::left_join(a1, a2, by = intersect(names(a1), names(a2))) %>%
        dplyr::mutate(prey_w = dplyr::if_else(sum_prey_w > 0, prey_w/sum_prey_w, 0), sum_prey_w = NULL)
    attr(b, "aggregated") <- "strata relativ stomach contents"

    return(dplyr::ungroup(b))
}


