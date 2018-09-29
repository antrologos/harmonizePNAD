
build_work_socialSecurityContrib_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_socialSecurityContrib.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_socialSecurityContrib)

        # Recoding
        Data[ , socialSecurityContrib := as.numeric(NA)]

        expr_contrib   <- with(crosswalk_i, paste(var_socialSecurityContrib,"%in% c(",socialSecurityContrib_yes, ")"))
        expr_notContr  <- with(crosswalk_i, paste(var_socialSecurityContrib,"%in% c(",socialSecurityContrib_no, ")"))

        Data[eval(parse(text = expr_contrib)),   socialSecurityContrib := 1]
        Data[eval(parse(text = expr_notContr)),  socialSecurityContrib := 0]

        gc()

        Data

}
