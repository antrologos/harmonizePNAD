
build_work_occupationalStatus_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_occupationalStatus.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_occupationalStatus)

        # Recoding
        Data[ , occupationalStatus := as.numeric(NA)]

        occupationalStatus_occupied    <- with(crosswalk_i, paste(var_occupationalStatus,"%in% c(",occupationalStatus_occupied, ")"))
        occupationalStatus_disoccupied <- with(crosswalk_i, paste(var_occupationalStatus,"%in% c(",occupationalStatus_disoccupied, ")"))

        Data[eval(parse(text = occupationalStatus_occupied)),    occupationalStatus := 1]
        Data[eval(parse(text = occupationalStatus_disoccupied)), occupationalStatus := 0]

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "age",
                                                         general_or_specific = "general")

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "econActivity",
                                                         general_or_specific = "general")

        Data[age < 10,                                occupationalStatus := NA]
        Data[is.na(econActivity) | econActivity == 0, occupationalStatus := NA]
        Data <- harmonizePNAD:::erase_just_created_vars(Data)

        gc()

        Data
}
