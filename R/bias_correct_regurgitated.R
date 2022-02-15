#' Regurgitated stomachs are from feeding fish, but their actual absolute stomach contents (in the sea) are not known.
#' Stomach contents from regurgitated stomach are not analysed or included as data, however the number
#' of regurgitated stomachs (n_regur) should be noted together with the number of stomachs with skeletal remains only (n_nskel) and the number of empty stomachs (n_empty).
#' The bias correction assumes that the regurgitated stomachs have had a the same stomach contents as the observed feeding fish (n_food).
#'
#' Ideally each stomach should be classified as: with food, with food but
#' regurgitated, with skeletal remains only or empty. This information is
#' included in the presently used exchange format.
#'
#' \tabular{ll}{
#'   \strong{Variable } \tab \strong{Comments} \cr
#' n_food  \tab Stomachs with (recently ingested) food\cr
#' n_regur \tab Stomach with food, but evidence of regurgitation of parts or all the stomach contents\cr
#' n_skel  \tab Stomachs with practically indigestible remains and the fish is not considered feeding recently\cr
#' n_empt  \tab No stomach contents \cr
#' }
#'
#' Stomach contents from regurgitated (or everted) stomachs should not be
#' included in the estimation of diet as the proportion regurgitated is
#' unknown. The number of fish with regurgitated stomachs may however be
#' recorded ('n_regur'). With the assumption that the
#' regurgitated stomachs sampled within 'sample_id' have had the same
#' stomach contents as the feeding (non-regurgitated) fish each observed
#' prey item weight and number can be corrected by a factor to calculate
#' the mean stomach contents of a predator within a 'sample_id'.
#'
#'
#' \deqn{factor = \frac{(N_{food} + N_{regur})}{N_{food}*(N_{food} + N_{skel} + N_{regur} + N_{empt})}}
#'
##'
#' @title Bias correct due to regurgitated stomachs
#'
#' @param stom  stomach contents data of class STOMobs.
#' @param delete_just_regurgitated Delete stomachs where all stomachs within a sample_id and predator size class are regurgitated.
#' @param drop_variables  Names of variables for number of stomachs to be deleted from dataset. This includes combinations of 'n_food', 'n_regur', 'n_skel' and 'n_empty'.
#' if drop_variables is missing the variables 'n_food', 'n_regur', 'n_skel' and 'n_empty' will be deleted.
#' @param update_n_food_with_n_regur Update number of stomachs with food (n_food) by the number of regurgitated stomachs (n_regur).
#' @return stomach contents data of class STOMobs, where the stomach contents per sample have been raised as it is assumed
#' that the regurgitated stomach had the same stomach contents as the one with food (and no regurgitation).
#' @export
#' @examples \dontrun{a <- bias_correct_regurgitated(stom=a,delete_just_regurgitated=TRUE,
#'           drop_variables=c('n_skel','n_empty', 'n_regur'))}
bias_correct_regurgitated <- function(stom, delete_just_regurgitated = TRUE, drop_variables, update_n_food_with_n_regur = TRUE) {
  fish_id<-just_regur<-n_empty<-n_food<-n_regur<-n_skel<-n_tot<-pred_size<-prey_n<-prey_w<-regur_fac<-sample_id<-NULL
    # stom<-a; delete_just_regurgitated=TRUE

    if (attr(stom, all_stom_attributes()["prey_w_id"]) == FALSE & "prey_w_meth" %in% colnames(stom))
        stop("ERROR: You have to run prey_w_from_pooled_weight() first \n")


    if (!("pred_size" %in% colnames(stom[["PRED"]]))) {
        cat("ERROR: Dataset does not include the variable for predator size class (pred_size).\nYou have to run put_size_class_on_predator() first!\n")
        stop()

    }

    fac <- stom[["PRED"]] %>%
        dplyr::select(sample_id, pred_size, n_tot, n_food, n_regur, n_skel, n_empty) %>%
        dplyr::group_by(sample_id, pred_size) %>%
        dplyr::summarise(n_food = sum(n_food), n_regur = sum(n_regur), n_tot = sum(n_tot), n_skel = sum(n_skel)) %>%
        dplyr::mutate(regur_fac = ifelse((n_skel == n_tot) | (n_food == 0), 0, (n_food + n_regur)/n_food)) %>%
        dplyr::mutate(just_regur = n_regur == n_tot, n_food = NULL, n_regur = NULL, n_tot = NULL, n_skel = NULL)
    fac2 <- dplyr::left_join(dplyr::select(stom[["PRED"]], sample_id, fish_id, pred_size), fac, by = c("sample_id", "pred_size")) %>%
        dplyr::mutate(pred_size = NULL)

    stom[["PREY"]] <- dplyr::left_join(stom[["PREY"]], fac2, by = c("sample_id", "fish_id")) %>%
        dplyr::mutate(prey_w = prey_w * regur_fac, prey_n = prey_n * regur_fac) %>%
        dplyr::mutate(regur_fac = NULL, prey_w_meth = NULL)

    if (delete_just_regurgitated) {
        stom[["PREY"]] <- dplyr::filter(stom[["PREY"]], !just_regur) %>%
            dplyr::select(-just_regur)
        del <- dplyr::filter(fac2, just_regur) %>%
            dplyr::select(-regur_fac, -just_regur)
        stom[["PRED"]] <- dplyr::anti_join(stom[["PRED"]], del, by = c("sample_id", "fish_id"))
    }
    if (update_n_food_with_n_regur)
        stom[["PRED"]] <- dplyr::mutate(stom[["PRED"]], n_food = as.integer(n_food + n_regur))

    # delete
    if (missing(drop_variables)) {
        stom[["PRED"]] <- dplyr::select(stom[["PRED"]], -n_regur, -n_food, -n_skel, -n_empty)
    } else {
        for (v in drop_variables) if (v %in% c("n_food", "n_regur", "n_skel", "n_empty"))
            stom[["PRED"]][, v] <- NULL
    }

    attr(stom, all_stom_attributes()["bias_regur"]) <- TRUE
    return(stom)
}

