

build_income_earningsMainJob_pnad <- function(Data){

        metadata = harmonizePNAD:::get_metadata(Data)

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_earningsMainJob.csv",
                                     package = "harmonizePNAD")
        crosswalk <- data.table::fread(file_location, colClasses = "numeric", dec =",")
        crosswalk <- crosswalk[year == metadata$year]

        harmonizePNAD:::check_necessary_vars(Data = Data, crosswalk$var_earningsMainJob)

        Data$earningsMainJob = Data[[crosswalk$var_earningsMainJob]]


        if(metadata$year %in% 1976:1990){
                Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                                 var_name = "occupationalStatus",
                                                                 general_or_specific = "general")

                Data[occupationalStatus == 1 & is.na(earningsMainJob), earningsMainJob := 0]
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        if(metadata$year %in% 1992:2001){
                Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                                 var_name = "occupationalStatus",
                                                                 general_or_specific = "general")

                Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                                 var_name = "econActivity",
                                                                 general_or_specific = "general")

                Data[occupationalStatus == 0, earningsMainJob := NA]
                Data[econActivity == 0      , earningsMainJob := NA]

                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }


        Data[earningsMainJob >= crosswalk$missing_values, earningsMainJob := NA]



        Data
}

