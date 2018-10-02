#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_geography_region2010standard <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        metadata = harmonizePNAD:::get_metadata(Data)

        if((metadata$type == "pnad" & metadata$year <= 1990) |
           (metadata$type == "census" & metadata$year == 1960)){
                warning(paste("It is not possible to produce 'region2010standard' for",
                              metadata$type,
                              metadata$year))
                return(Data)
        }

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "state2010standard",
                                                         general_or_specific = "general")
        gc()

        Data[, region2010standard := trunc(state2010standard/10)]

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }


        Data
}

