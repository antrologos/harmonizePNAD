#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_geography_stateMCA <- function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_geography_stateMCA_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        gc(); Sys.sleep(.3); gc()

        Data
}

