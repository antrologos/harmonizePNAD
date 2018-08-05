build_demographics_age_pnad <- function(Data){

        metadata = harmonizePNAD:::get_metadata(Data)

        year_var <- data.frame(
                year = c(1973, 1976:1979, 1981:1990, 1992, 1993, 1995:1999, 2001:2009, 2011:2015),
                var  = c("v0156", "v2105", "v0173", "v2805", "v2805", rep("v0805", 10), rep("v8005",21)),
                stringsAsFactors = F
        )

        year_i = which(year_var == metadata$year)
        var_i  = year_var[year_i, "var"]

        harmonizePNAD:::check_necessary_vars(Data, var_i)
        Data[, age := eval(parse(text = var_i))]

        Data[age == 999, age := NA]

        if(metadata$year == 1973) {
                warning("PNAD 1973: information only for persons aged 10+. Age is truncated at 99 years")
        }

        if(metadata$year == 1976) {
                warning("PNAD 1976: age is truncated at 99 years")
        }

        Data
}
