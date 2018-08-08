build_demographics_race2010standard_pnad <- function(Data){

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
        Data[ , race2010standard := as.numeric(NA)]

        expr_white      <- with(crosswalk_i, paste(var_race,"==",race_white))
        expr_black      <- with(crosswalk_i, paste(var_race,"==",race_black))
        expr_mixed      <- with(crosswalk_i, paste(var_race,"==",race_mixed))
        expr_Indigenous <- with(crosswalk_i, paste(var_race,"==",race_indigenous))
        expr_asian      <- with(crosswalk_i, paste(var_race,"==",race_asian))
        expr_unknown    <- with(crosswalk_i, paste(var_race,"==",race_Unknown))

        Data[eval(parse(text = expr_white)),      race2010standard := 2]
        Data[eval(parse(text = expr_black)),      race2010standard := 4]
        Data[eval(parse(text = expr_asian)),      race2010standard := 6]
        Data[eval(parse(text = expr_mixed)),      race2010standard := 8]
        Data[eval(parse(text = expr_Indigenous)), race2010standard := 0]
        Data[eval(parse(text = expr_unknown)),    race2010standard := 9]
        gc()

        Data
}

