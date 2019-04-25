#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export
build_income_incomeOtherSources <- function(Data, deflate_values = T){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        metadata = harmonizePNAD:::get_metadata(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_income_incomeOtherSources_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        if(deflate_values == T){
                # Loading the crosswalk
                file_location <- system.file("extdata",
                                             "crosswalk_pnad_incomeOtherSources.csv",
                                             package = "harmonizePNAD")
                crosswalk <- data.table::fread(file_location, colClasses = "numeric", dec = ",")
                crosswalk <- crosswalk[year == metadata$year]

                Data[ , incomeOtherSources :=  incomeOtherSources*crosswalk$deflator]
        }

        gc()
        Data
}
