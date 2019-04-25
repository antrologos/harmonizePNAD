build_education_levelAttend_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_levelAttend.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)

        selected_lines <- NULL
        for(i in 1:nrow(crosswalk)){
                test <- crosswalk[i , metadata$year %in% eval(parse(text = year))]

                if(test == TRUE){
                        selected_lines <- c(selected_lines, i)
                }
        }
        crosswalk_i <- crosswalk[selected_lines]

        # Checking the variable availability
        necessary_vars <- dplyr::select(crosswalk_i, starts_with("var")) %>%
                as.matrix() %>%
                as.character() %>%
                unique()

        necessary_vars <- necessary_vars[nchar(necessary_vars) > 1]

        harmonizePNAD:::check_necessary_vars(Data, necessary_vars)

        # Recoding
        Data[ , levelAttend := as.numeric(NA)]

        for(j in 1:nrow(crosswalk_i)){
                var_grau      = crosswalk_i[j,]$var_grau
                var_serie     = crosswalk_i[j,]$var_serie
                var_condition = crosswalk_i[j,]$var_condition

                value_grau      = crosswalk_i[j,]$value_grau
                value_serie     = crosswalk_i[j,]$value_serie
                value_condition = crosswalk_i[j,]$value_condition

                finalvalue = crosswalk_i[j,]$finalvalue %>% as.numeric()

                exp_condition = paste(var_grau,"%in% c(",value_grau,")")

                if(nchar(var_serie) > 1){
                        exp_condition <- paste(exp_condition, "&", var_serie,"%in% c(",value_serie,")")
                }

                if(nchar(var_condition) > 1){
                        exp_condition <- paste(exp_condition, "&", var_condition,"%in% c(",value_condition,")")
                }

                Data[eval(parse(text = exp_condition)),    levelAttend := finalvalue]
        }
        gc()

        Data
}


