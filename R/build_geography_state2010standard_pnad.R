build_geography_state2010standard_pnad <- function(Data){

        metadata = harmonizePNAD:::get_metadata(Data)

        if(metadata$year <= 1990){
                warning(paste("It is not possible to produce 'state2010standard' for PNAD",metadata$year))
                return(Data)
        }

        harmonizePNAD:::check_necessary_vars(Data = Data, "uf")

        Data[, state2010standard := uf]

        Data
}

