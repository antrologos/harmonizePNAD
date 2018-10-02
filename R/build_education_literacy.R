#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_education_literacy <- function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_education_literacy_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        gc(); Sys.sleep(.3); gc()

        Data

}

