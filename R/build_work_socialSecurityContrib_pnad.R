
build_work_socialSecurityContrib_pnad <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

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

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "occupationalStatus",
                                                         general_or_specific = "general")

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "econActivity",
                                                         general_or_specific = "general")

        Data[is.na(occupationalStatus) | occupationalStatus == 0, socialSecurityContrib := NA]
        Data[is.na(econActivity)       | econActivity == 0      , socialSecurityContrib := NA]

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        gc()

        Data

}
