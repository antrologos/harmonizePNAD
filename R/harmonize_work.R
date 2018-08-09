#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

harmonize_work <- function(Data){
        harmonization_functions <- harmonizePNAD:::list_available_harmonizations("work")

        for(harmonization_function in harmonization_functions){
                print(harmonization_function)
                call <- paste0(harmonization_function,"(Data)")
                Data <- eval(parse(text = call))
        }
        Data
}
