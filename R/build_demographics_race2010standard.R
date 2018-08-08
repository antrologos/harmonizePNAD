#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_race2010standard <- function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        metadata = harmonizePNAD:::get_metadata(Data)
        if((metadata$type == "pnad" & metadata$year <= 1990) |
           (metadata$type == "census" & metadata$year %in% c(1970, 1980))){
                warning(paste("It is not possible to produce 'race2010standard' for",
                              metadata$type,
                              metadata$year))
                return(Data)
        }

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_demographics_race2010standard_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        gc(); Sys.sleep(.3); gc()

        Data
}
