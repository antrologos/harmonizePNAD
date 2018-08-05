#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_education_yearsSchooling <- function(Data){
        Data <- check_prepared_to_harmonize(Data)

        sulfix <- find_sulfix(Data)
        call <- paste0("build_education_yearsSchooling_", sulfix, "(Data)")
        Data <- eval(parse(text = call))

        gc(); Sys.sleep(.3); gc()

        Data
}
