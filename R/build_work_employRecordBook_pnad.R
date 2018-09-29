
build_work_employRecordBook_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_employRecordBook.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_employRecordBook)

        # Recoding
        Data[ , employRecordBook := as.numeric(NA)]

        expr_yes <- with(crosswalk_i, paste(var_employRecordBook,"%in% c(",employRecordBook_yes, ")"))
        expr_no  <- with(crosswalk_i, paste(var_employRecordBook,"%in% c(",employRecordBook_no, ")"))
        expr_publicServant_Military  <- with(crosswalk_i, paste(var_employRecordBook,"%in% c(",publicServant_Military, ")"))

        Data[eval(parse(text = expr_yes)), employRecordBook := 1]
        Data[eval(parse(text = expr_no)),  employRecordBook := 0]
        Data[eval(parse(text = expr_publicServant_Military)),  employRecordBook := 2]

        gc()

        Data
}
