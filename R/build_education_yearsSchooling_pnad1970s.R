# Calculo dos anos de estudo para as PNADs 1976-1979
build_education_yearsSchooling_pnad1970s <- function(Data){

        metadata <- harmonizePNAD:::get_metadata(Data)

        if(metadata$year == 1976)
                vars <- c("v2224", "v2225", "v2226", "v2227")

        if(metadata$year == 1977)
                vars <- c("v0035", "v0036", "v0037", "v0038")

        if(metadata$year %in% c(1978,1979))
                vars <- c("v2213", "v2214", "v2215", "v2216")

        harmonizePNAD:::check_necessary_vars(Data = Data, vars)

        vars1980s <-c("v0312", "v0314", "v0315", "v0317")

        exists_same_name_vars = any(vars1980s %in% names(Data))
        if(exists_same_name_vars){
                names(Data)[names(Data) %in% vars1980s] = paste0("tmp_", names(Data)[names(Data) %in% vars1980s])
        }


        if(metadata$year %in% 1976:1978){
                setnames(Data, old=vars, new=c("serie_f","grau_f","serie_nf","grau_nf"))

                # Levando a "serie que frequenta" para o modelo aplicano nos anos 1980
                Data[ , v0312 := serie_f]

                # Levando o "Grau que frequenta" para o modelo aplicano nos anos 1980
                Data[ grau_f == 7  , v0314 := 1]
                Data[ grau_f == 3  , v0314 := 2]
                Data[ grau_f == 4  , v0314 := 3]
                Data[ grau_f == 1  , v0314 := 4]
                Data[ grau_f == 2  , v0314 := 5]
                Data[ grau_f == 5  , v0314 := 6]
                Data[ grau_f == 6  , v0314 := 8]
                Data[ grau_f == 9  , v0314 := 9]
                Data[ grau_f == 10 , v0314 := 10]
                Data[ grau_f == 8  , v0314 := 13]
                Data[ grau_f == 11 , v0314 := 14]
                Data[ grau_f == 0  , v0314 := 99]
                Data[ grau_f == 99 , v0314 := 99]

                # Levando a "serie que nao frequenta" para o modelo aplicano nos anos 1980
                Data[ , v0315 := serie_nf]

                # Levando o "Grau que nao frequenta" para o modelo aplicano nos anos 1980
                Data[ grau_nf == 3 , v0317 := 2]
                Data[ grau_nf == 4 , v0317 := 3]
                Data[ grau_nf == 1 , v0317 := 4]
                Data[ grau_nf == 2 , v0317 := 5]
                Data[ grau_nf == 5 , v0317 := 6]
                Data[ grau_nf == 9 , v0317 := 99]
                Data[ grau_nf == 0 , v0317 := 99]

                setnames(Data, old=c("serie_f","grau_f","serie_nf","grau_nf"), new=vars)
        }


        if(metadata$year == 1979){
                # Codificacao ja segue o padrao dos anos 1980. Entao basta renomear
                Data[ , v0312 := v2213]
                Data[ , v0314 := v2214]
                Data[ , v0315 := v2215]
                Data[ , v0317 := v2216]
        }

        Data <- harmonizePNAD:::build_education_yearsSchooling_pnad1980s(Data)
        gc()

        Data[ , v0312 := NULL]
        Data[ , v0314 := NULL]
        Data[ , v0315 := NULL]
        Data[ , v0317 := NULL]

        if(exists_same_name_vars){
                tmp_vars <- paste0("tmp_", vars1980s)
                tmp_vars <- tmp_vars[which(tmp_vars %in% names(Data))]
                names(Data)[names(Data) %in% tmp_vars] <- gsub(x = tmp_vars,
                                                               pattern = "tmp_",
                                                               replacement = "")
        }

        Data

}
