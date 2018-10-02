
build_geography_ruralUrban_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_ruralUrban.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location, colClasses = "character")

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        if(metadata$year %in% c(1976, 1978, 1979)){
                warning(paste0("For the 1976, 1978, and 1979 editions, the variable about rural/urban\n",
                               "situation is not originally contained in the persons data file. You\n",
                               "must import it from the household data file first."))
        }
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_ruralUrban)

        # Recoding
        Data[ , ruralUrban := as.numeric(NA)]

        expr_rural <- with(crosswalk_i, paste(var_ruralUrban,"%in% c(",value_rural, ")"))
        expr_urban <- with(crosswalk_i, paste(var_ruralUrban,"%in% c(",value_urban, ")"))

        Data[eval(parse(text = expr_rural)), ruralUrban := 0]
        Data[eval(parse(text = expr_urban)), ruralUrban := 1]

        gc()

        Data
}
