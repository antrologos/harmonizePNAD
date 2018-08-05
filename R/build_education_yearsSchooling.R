#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_education_yearsSchooling <- function(Data){

        Data   <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "specific")

        if(sulfix == "pnad1973"){
                warning("PNAD 1973: It is not possible to build a variable for 'years of schoolling' for the year 1973.")
        }else{
                call   <- paste0("harmonizePNAD:::build_education_yearsSchooling_", sulfix, "(Data)")
                Data   <- eval(parse(text = call))
        }

        gc(); Sys.sleep(.3); gc()

        Data
}
