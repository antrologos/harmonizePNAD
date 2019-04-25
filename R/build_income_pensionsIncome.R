#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export
build_income_pensionsIncome <- function(Data, deflate_values = T){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        metadata = harmonizePNAD:::get_metadata(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_income_pensionsIncome_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        if(deflate_values == T){
                # Loading the crosswalk
                file_location <- system.file("extdata",
                                             "crosswalk_pnad_pensionsIncome.csv",
                                             package = "harmonizePNAD")
                crosswalk <- data.table::fread(file_location, colClasses = "numeric", dec = ",")
                crosswalk <- crosswalk[year == metadata$year]

                Data[ , pensionsIncome :=  pensionsIncome*crosswalk$deflator]
        }

        gc()
        Data
}

