#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_identification_year <- function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        metadata <- harmonizePNAD:::get_metadata(Data)

        Data[, year := as.numeric(metadata$year)]

        Data
}
