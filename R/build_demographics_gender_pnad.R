build_demographics_gender_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_gender.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_gender)

        # Recoding
        Data[ , gender_male := as.numeric(NA)]

        expr_male   <- with(crosswalk_i, paste(var_gender,"==",gender_male))
        expr_female <- with(crosswalk_i, paste(var_gender,"==",gender_female))

        Data[eval(parse(text = expr_male)),    gender_male := 1]
        Data[eval(parse(text = expr_female)) , gender_male := 0]

        Data
}


