#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_raceDummy <- function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        metadata = harmonizePNAD:::get_metadata(Data)
        if((metadata$type == "pnad" & metadata$year <= 1986) |
           (metadata$type == "census" & metadata$year == 1970)){
                warning(paste("It is not possible to produce 'race1980standard' for",
                              metadata$type,
                              metadata$year))
                return(Data)
        }

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "race1980standard",
                                                         general_or_specific = "general")


        Data[, raceDummy := as.numeric(NA)]
        Data[race1980standard %in% c(2, 6), raceDummy := 1]
        Data[race1980standard %in% c(4, 8), raceDummy := 0]

        Data <- harmonizePNAD:::erase_just_created_vars(Data)
        gc(); Sys.sleep(.3); gc()

        Data
}

