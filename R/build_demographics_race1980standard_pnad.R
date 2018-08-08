build_demographics_race1980standard_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_race.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)

        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_race)

        # Recoding
        Data[ , race1980standard := as.numeric(NA)]

        expr_white    <- with(crosswalk_i, paste(var_race,"==",race_white))
        expr_black    <- with(crosswalk_i, paste(var_race,"==",race_black))
        expr_asian    <- with(crosswalk_i, paste(var_race,"==",race_asian))
        expr_unknown  <- with(crosswalk_i, paste(var_race,"==",race_Unknown))

        if(metadata$year <= 1990){
                expr_mixedOrIndigenous  <- with(crosswalk_i, paste(var_race,"==",race_mixedOrIndigenous))
        }else{
                expr_mixedOrIndigenous  <- with(crosswalk_i, paste(var_race,"==",race_mixed,
                                                                   "|",
                                                                   var_race,"==",race_indigenous))
        }

        Data[eval(parse(text = expr_white)),             race1980standard := 2]
        Data[eval(parse(text = expr_black)),             race1980standard := 4]
        Data[eval(parse(text = expr_asian)),             race1980standard := 6]
        Data[eval(parse(text = expr_mixedOrIndigenous)), race1980standard := 8]
        Data[eval(parse(text = expr_unknown)),           race1980standard := 9]

        gc()

        Data
}


