#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_age <- function(Data){

        Data <- check_prepared_to_harmonize(Data)

        sulfix <- find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_demographics_age_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        gc(); Sys.sleep(.3); gc()

        Data

}


