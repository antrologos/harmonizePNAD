#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_geography_regionMCA <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "stateMCA",
                                                         general_or_specific = "general")

        Data[ , regionMCA := trunc(stateMCA/10)]
        gc(); Sys.sleep(.3); gc()

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        Data
}

