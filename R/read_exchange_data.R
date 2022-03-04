#' Read data from exchange data
#' @title Read stomach contents data on exchange format
#' @param control Options \link{STOMcontrol-class}) for dplyr::selecting and collating stomach data.
#' @param delete_errors  Logical for deleting records with errors.
#' if FALSE, the function will stop at the first error found,
#' if TRUE records with errors will be deleted, but the function continues if possible.
#' @param allow_alias_names Logical for using alias field names as specified in the stomach_format
#' @param keep_just_mandatory_fields Logical for just keeping mandatory field names as specified in the stomach_format
#' @return Stomach data of class STOMobs.
#' @importFrom readr read_csv
#' @export
#' @examples \dontrun{read_exchange_data(control)}
#' @field a1 her er a1
#' @field a2 her er a2
read_exchange_data <- function(control, delete_errors = FALSE, allow_alias_names = FALSE, keep_just_mandatory_fields = FALSE) {

    alias_1<-alias_2<-alias_3<-dataset<-field<-fish_id<-mandatory<-n_sample_id<-pred_l<-pred_ll<-pred_lu<-pred_name<-sample_id<-types<-year<-NULL

    options(dplyr.summarise.inform = FALSE)
    stom_dir <- control@stomach_dir
    exchange_file <- control@dataSets
    if (is.na(exchange_file[1]))
        stop("\nError Input file name for stomach data on exchange format must be given in the control@datasets\n")

    stomach_format <- eval(control@stomach_format)
    b <- read.csv(file = stomach_format, stringsAsFactors = FALSE)

    if (allow_alias_names)
        b <- subset(b, select = c(field, types, mandatory, alias_1, alias_2, alias_3)) else b <- subset(b, select = c(field, types, mandatory))

    all_fields <- b$field
    mandatory_names <- subset(b, mandatory)$field

    # read stomach data using type definitions
    a <- lapply(exchange_file, function(x) {
        # test x<-exchange_file[1]
        cat("\nProcessing file_", x, "\n")
        a <- readr::read_csv(file = file.path(stom_dir, x), n_max = 1, col_types = readr::cols())
        name_in <- colnames(a)

        if (allow_alias_names) {
            b$alias_4 <- b$field
            any_name <- unique(c(b$alias_1, b$alias_2, b$alias_3, b$alias_4))
            any_name <- any_name[any_name != ""]

            # if (length(setdiff(name_in, any_name)) > 0) { cat(paste0('File: ', x, ' includes variable names\n', paste(setdiff(name_in, any_name), collapse = ', '), '\n
            # which are not included as a valid variable name in file:', stomach_format,'. \n The invalid variable is not included in the resulting data set.\n')) }

            found <- matrix(0, nrow = 4, ncol = length(name_in))
            colnames(found) <- name_in

            for (i in (1:4)) found[i, ] <- match(name_in, with(b, get(paste0("alias_", i))))

            key <- colMeans(found, na.rm = TRUE)
            name_in <- all_fields[key]
        }

        if (!setequal(mandatory_names, intersect(name_in, dplyr::all_of(mandatory_names)))) {
            stop(paste0("File: ", x, " does not include the mandatory field:", setdiff(mandatory_names, intersect(name_in, mandatory_names)), "\n"))
        }



        if (length(setdiff(name_in, all_fields)) > 0) {
            cat(paste0("File: ", x, " includes variable name\n", paste(setdiff(name_in, all_fields), collapse = ", "), "\n which is not considered as a valid variable name and is not included in file:",
                stomach_format))
            if (!delete_errors)
                stop("remove not included variables, or rerun with parameter delete_errors=TRUE")
            cat("\n The invalid variable is not included in the resulting data set.\n")
        }

        if (!allow_alias_names) {
            b <- subset(b, field %in% name_in)
            key <- match(name_in, b$field)
        }

        types <- paste(b[key, "types"], collapse = "")
        types <- gsub("NA", "-", types)
        a <- readr::read_csv(file = file.path(stom_dir, x), col_types = types, na = c("", "NA", "NULL", "-999", "-9", "-99"))  # read data with specified data type
        coln <- b[key, "field"]
        colnames(a) <- coln[!is.na(coln)]
        return(a)
    })
    # combine them into one
    a <- do.call(dplyr::bind_rows, a)


    mis_pl <- is.na(a$pred_l) & is.na(a$pred_ll) & is.na(a$pred_lu)

    if (any(mis_pl)) {
        if (delete_errors) {
            a <- a[!mis_pl, ]
            cat("Records with neither pred_l, pred_ll or pred_lu information have been deleted\n")
        } else {
            cat("Records with neither pred_l, pred_ll or pred_lu information\n")
            print(a[mis_pl, ] %>%
                dplyr::select(dataset, year, sample_id, pred_name, pred_l, pred_ll, pred_lu))
            ("read_exchange_data terminated due to errors in stomach data. Please corrct data or chose parameter delete_errors=TRUE")
        }
    }


    if (keep_just_mandatory_fields)
        a <- dplyr::select(a, dplyr::all_of(mandatory_names))

    ## convert all strings to factors
    strings_to_factors <- function(x) {
        x %>%
            dplyr::mutate_if(is.character, as.factor)
    }

    strings_to_factors_NA <- function(x) {
        x %>%
            dplyr::mutate_if(is.character, forcats::fct_explicit_na)
    }

    a <- strings_to_factors_NA(a)

    # divide into two data sets, for creation of class STOMobs
    cols <- colnames(a)
    b <- read.csv(file = stomach_format, stringsAsFactors = FALSE)
    pred <- subset(b, PRED == TRUE, select = field)$field
    incl <- intersect(cols, pred)
    PRED <- a %>%
        dplyr::select(dplyr::all_of(incl)) %>%
        dplyr::distinct() %>%
        dplyr::arrange(sample_id, fish_id)

    a1 <- PRED %>%
        dplyr::group_by(sample_id, fish_id) %>%
        dplyr::summarise(n_sample_id = dplyr::n()) %>%
        dplyr::filter(n_sample_id > 1)
    if (dim(a1)[[1]] > 0) {
        a3 <- dplyr::left_join(a1, PRED)
        cat("the following records do not have a unique combination of sample_id and fish_id\n")
        print(a3)
        stop()
        a3
    }

    prey <- subset(b, PREY == TRUE, select = field)$field
    incl <- intersect(cols, prey)
    PREY <- a %>%
        dplyr::select(all_of(incl)) %>%
        dplyr::arrange(sample_id, fish_id)
    a <- list(PRED = PRED, PREY = PREY)

    ## assign attributes
  #  attr(a,'PRED')<-names(a[['PRED']])
  #  attr(a,'PREY')<-names(a[['PREY']])

    for (at in all_stom_attributes()) {
        attr(a, at) <- FALSE
    }
    attr(a, "control") <- control
    class(a) <- "STOMobs"

    a<-update(a)
    if (!is.na(control@predators[1])) a<-subset(a,pred_name %in% control@predators )
    if (!is.na(control@years[1])) a<-subset(a,year %in% control@years )
    if (!is.na(control@quarters[1])) a<-subset(a,quarter %in% control@quarters )

    b <- check_unique_sample_id(a)
    if (dim(b)[[1]] > 0) {
        print(data.frame(b))
        print(xtabs(~year + country, data = b))
        stop("Duplicated combinations of sample_id and fish_id may have been found, please check!")
    }

    return(a)
}


#' Write data on exchange data
#' @title Write stomach contents data of class STOMobs on the exchange format.
#' @param stom Input stomach data set for writing
#' @param exchange_file  File name for output data on exchange format.
#' @return Returns the input stom invisibly.
#' @examples \dontrun{write_exchange_data(tst, exchange_file = 'cod_stomachs_2017.csv')}
#' @export
write_exchange_data <- function(stom, exchange_file = NA) {
    if (is.na(exchange_file))
        stop("\nError Input file name for stomach data on exchange format must be given\n")

    readr::write_csv(as.data.frame(stom), file = exchange_file)
}

