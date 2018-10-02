
build_education_schoolattnd_pnad <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_schoolattnd.csv",
                                     package = "harmonizePNAD")

        #file_location = "C:/Users/Rogerio/Google Drive/RCodes/PacotesR/harmonizePNAD/inst/extdata/crosswalk_pnad_schoolattnd.csv"
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_schoolattnd)

        # Recoding
        Data[ , schoolattnd := as.numeric(NA)]


        if(metadata$year %in% 1976:1979){

                expr_yes <- with(crosswalk_i, paste(var_schoolattnd,    "%in% c(",value_yes, ") & !(",
                                                    var_schoolattnd_aux, " %in% c(",value_no, ") )"))
                expr_no  <- with(crosswalk_i, paste(var_schoolattnd_aux,"%in% c(",value_no , ") & ",
                                                    var_schoolattnd,    "%in% c(",value_no, ")"
                                                    ))

                Data[eval(parse(text = expr_yes)), schoolattnd := 1]
                Data[eval(parse(text = expr_no)), schoolattnd  := 0]
        }

        if(metadata$year >= 1981){
                expr_no  <- with(crosswalk_i, paste(var_schoolattnd,"%in% c(",value_no, ")"))
                expr_yes <- with(crosswalk_i, paste(var_schoolattnd,"%in% c(",value_yes, ")"))

                Data[eval(parse(text = expr_yes)), schoolattnd := 1]
                Data[eval(parse(text = expr_no)),  schoolattnd := 0]
        }

        if(metadata$year == 1976){
                Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                                 var_name = "age",
                                                                 general_or_specific = "general")

                Data[age < 5 , schoolattnd := NA]

                if(just_created_vars_list_existedBefore == F){
                        Data <- harmonizePNAD:::erase_just_created_vars(Data)
                }
        }

        gc()

        Data
}
