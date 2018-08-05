build_identification_wgt_pnad <- function(Data){

        metadata = harmonizePNAD:::get_metadata(Data)

        if(metadata$year == 1973) {
                warning("PNAD 1973: We assume the Weight Variable is named 'p_est_e' and that it refers to 'Peso Estado (Amostra Expandida)', according to the official documentation")
        }

        year_var <- data.frame(
                year = c(1973, 1976:1979, 1981:1989, 1990, 1992, 1993, 1995:1999, 2001:2009, 2011:2015),
                var  = c("p_est_e", "v2985", "v0187", "v2997", "v2999", rep("v9991", 9), "v3091", rep("v4729",21)),
                stringsAsFactors = F
        )

        year_i = which(year_var == metadata$year)
        var_i  = year_var[year_i, "var"]

        harmonizePNAD:::check_necessary_vars(Data, var_i)
        Data[, wgt := eval(parse(text = var_i))]

        Data
}

