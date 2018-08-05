build_geography_stateMCA_pnad <- function(Data){

        metadata = harmonizePNAD:::get_metadata(Data)

        year_var <- data.frame(
                year = c(1973, 1976:1979, 1981:1990, 1992, 1993, 1995:1999, 2001:2009, 2011:2015),
                var  = c("v0152", "v0003", "v0002", "v0006", "v0017", rep("v0010", 10), rep("uf",21)),
                var2  = c("v0151", rep(NA, 35)),
                stringsAsFactors = F
        )

        year_i = which(year_var == metadata$year)

        if(metadata$year == 1973){
                var1  = year_var[year_i, "var"]
                var2  = year_var[year_i, "var2"]
                harmonizePNAD:::check_necessary_vars(Data, c(var1, var2))

                # In 1973, state = region*10 + state_one_digit
                Data[, state_tmp := eval(parse(text = paste(var2,"*10 + ",var1)))]
        }else{
                var_i  = year_var[year_i, "var"]
                harmonizePNAD:::check_necessary_vars(Data, var_i)
                Data[, state_tmp := eval(parse(text = var_i))]
        }


        Data[, stateMCA  := as.numeric(NA)]

        if(metadata$year == 1973){
                Data[state_tmp ==71, stateMCA  := 11]
                Data[state_tmp ==72, stateMCA  := 12]
                Data[state_tmp ==73, stateMCA  := 13]
                Data[state_tmp ==74, stateMCA  := 14]
                Data[state_tmp ==75, stateMCA  := 15]
                Data[state_tmp ==76, stateMCA  := 16]
                Data[state_tmp ==51, stateMCA  := 21]
                Data[state_tmp ==52, stateMCA  := 22]
                Data[state_tmp ==53, stateMCA  := 23]
                Data[state_tmp ==54, stateMCA  := 24]
                Data[state_tmp ==55, stateMCA  := 25]
                Data[state_tmp ==56, stateMCA  := 26]
                Data[state_tmp ==57, stateMCA  := 27]
                Data[state_tmp ==58, stateMCA  := 28]
                Data[state_tmp ==59, stateMCA  := 29]
                Data[state_tmp ==41, stateMCA  := 31]
                Data[state_tmp ==42, stateMCA  := 32]
                Data[state_tmp ==11, stateMCA  := 33]
                Data[state_tmp ==12, stateMCA  := 33]
                Data[state_tmp ==21, stateMCA  := 35]
                Data[state_tmp ==31, stateMCA  := 41]
                Data[state_tmp ==32, stateMCA  := 42]
                Data[state_tmp ==33, stateMCA  := 43]
                Data[state_tmp ==81, stateMCA  := 51] # Inclui Mato Grosso e Mato Grosso do Sul
                Data[state_tmp ==82, stateMCA  := 52] # Inclui Tocantis e Goias
                Data[state_tmp ==61, stateMCA  := 53]
        }

        if(metadata$year %in% 1976:1979){
                # recodifica os codigos dos anos de 1976 a 1979 conforme o padrao
                # adotado nos anos 1990, 2000 e 2010
                Data[state_tmp ==71, stateMCA  := 11]
                Data[state_tmp ==72, stateMCA  := 12]
                Data[state_tmp ==73, stateMCA  := 13]
                Data[state_tmp ==74, stateMCA  := 14]
                Data[state_tmp ==75, stateMCA  := 15]
                Data[state_tmp ==76, stateMCA  := 16]
                Data[state_tmp ==51, stateMCA  := 21]
                Data[state_tmp ==52, stateMCA  := 22]
                Data[state_tmp ==53, stateMCA  := 23]
                Data[state_tmp ==54, stateMCA  := 24]
                Data[state_tmp ==55, stateMCA  := 25]
                Data[state_tmp ==56, stateMCA  := 26]
                Data[state_tmp ==57, stateMCA  := 27]
                Data[state_tmp ==58, stateMCA  := 28]
                Data[state_tmp ==59, stateMCA  := 29]
                Data[state_tmp ==41, stateMCA  := 31]
                Data[state_tmp ==43, stateMCA  := 32]
                Data[state_tmp ==11, stateMCA  := 33]
                Data[state_tmp ==21, stateMCA  := 35]
                Data[state_tmp ==31, stateMCA  := 41]
                Data[state_tmp ==32, stateMCA  := 42]
                Data[state_tmp ==33, stateMCA  := 43]
                Data[state_tmp ==77, stateMCA  := 51]
                Data[state_tmp ==78, stateMCA  := 52]
                Data[state_tmp ==61, stateMCA  := 53]
        }


        if(metadata$year %in% 1981:1990){
                # recodifica os codigos dos anos 1980 conforme o padrao
                # adotado nos anos 1990, 2000 e 2010
                Data[state_tmp %in% 11:14, stateMCA  := 33]

                Data[state_tmp %in% 20:29, stateMCA  := 35]

                Data[state_tmp %in% c(30:31,37), stateMCA  := 41]
                Data[state_tmp == 32, stateMCA  := 42]

                Data[state_tmp %in% 33:35, stateMCA  := 43]

                Data[state_tmp %in% c(41:42,44), stateMCA  := 31]

                Data[state_tmp == 43, stateMCA  := 32]
                Data[state_tmp == 51, stateMCA  := 21]
                Data[state_tmp == 52, stateMCA  := 22]
                Data[state_tmp == 53, stateMCA  := 23]
                Data[state_tmp == 54, stateMCA  := 24]
                Data[state_tmp == 55, stateMCA  := 25]
                Data[state_tmp == 56, stateMCA  := 26]
                Data[state_tmp == 57, stateMCA  := 27]
                Data[state_tmp == 58, stateMCA  := 28]
                Data[state_tmp %in% 59:60, stateMCA  := 29]

                Data[state_tmp == 61, stateMCA  := 53]
                Data[state_tmp == 71, stateMCA  := 11]
                Data[state_tmp == 72, stateMCA  := 12]
                Data[state_tmp == 73, stateMCA  := 13]
                Data[state_tmp == 74, stateMCA  := 14]
                Data[state_tmp == 75, stateMCA  := 15]
                Data[state_tmp == 76, stateMCA  := 16]
                Data[state_tmp == 81, stateMCA  := 51] # Mato Grosso do Sul -> Mato Grosso
                Data[state_tmp == 82, stateMCA  := 51] # Mato Grosso        -> Mato Grosso
                Data[state_tmp == 83, stateMCA  := 52]
        }

        if(metadata$year >= 1992){
                Data[, stateMCA  := state_tmp]

                Data[state_tmp == 17, stateMCA  := 52] # funde Tocantins e Goias
                Data[state_tmp == 50, stateMCA  := 51] # funde Mato Grosso do Sul e Mato Grosso

        }
        gc()

        Data[, state_tmp := NULL]

        Data
}

