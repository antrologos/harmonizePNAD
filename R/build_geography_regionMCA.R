#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_geography_regionMCA <- function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "stateMCA",
                                                         general_or_specific = "general")

        Data[ , regionMCA := trunc(stateMCA/10)]
        gc(); Sys.sleep(.3); gc()

        Data <- harmonizePNAD:::erase_just_created_vars(Data)
        Data
}

