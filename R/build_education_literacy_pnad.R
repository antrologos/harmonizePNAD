build_education_literacy_pnad <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_literacy.csv",
                                     package = "harmonizePNAD")
        #file_location <- "C:/Users/Rogerio/Google Drive/RCodes/PacotesR/harmonizePNAD/inst/extdata/crosswalk_pnad_literacy.csv"
        crosswalk   <- data.table::fread(file_location, colClasses = "character")

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_literacy)

        # Recoding
        Data[ , literacy := as.numeric(NA)]

        expr_literate   <- with(crosswalk_i, paste(var_literacy,"%in% c(",value_literate, ")"))
        expr_illiterate <- with(crosswalk_i, paste(var_literacy,"%in% c(",value_illiterate, ")"))

        Data[eval(parse(text = expr_literate)),    literacy := 1]
        Data[eval(parse(text = expr_illiterate)),  literacy := 0]

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "age",
                                                         general_or_specific = "general")

        Data[age < 5, literacy := NA]

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        gc()

        Data
}


