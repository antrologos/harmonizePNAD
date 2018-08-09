
build_work_econActivity_pnad <- function(x){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_econActivity.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_econActivity)

        # Recoding
        Data[ , econActivity := as.numeric(NA)]

        expr_active   <- with(crosswalk_i, paste(var_econActivity,"%in% c(",econActivity_active, ")"))
        expr_inactive <- with(crosswalk_i, paste(var_econActivity,"%in% c(",econActivity_inactive, ")"))

        Data[eval(parse(text = expr_active)),    econActivity := 1]
        Data[eval(parse(text = expr_inactive)),  econActivity := 0]

        gc()

        Data


}
