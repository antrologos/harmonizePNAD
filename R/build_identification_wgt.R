#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_identification_wgt <- function(Data, typeOfweight = "fweight"){

        if(!(typeOfweight %in% c("fweight", "aweight", "pweight"))){
                stop("'typeOfweight' must be equal to 'fweight', 'aweight', or 'pweight'")
        }

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_identification_wgt_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        Data[, wgt := as.numeric(wgt)]
        gc()

        if(typeOfweight == "fweight"){
                Data[, fweight := wgt]
        }

        if(typeOfweight == "pweight"){
                Data[, pweight := (wgt/sum(wgt))]
        }

        if(typeOfweight == "aweight"){
                Data[, aweight := (wgt/sum(wgt))*nrow(Data)]
        }

        Data[ , wgt := NULL]
        gc(); Sys.sleep(.3); gc()

        Data
}
