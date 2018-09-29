build_work_hoursWorkedMainJob_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_hoursWorked.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)
        #crosswalk   <- data.table::fread("E:/Google Drive/RCodes/PacotesR/harmonizePNAD/inst/extdata/crosswalk_pnad_hoursWorked.csv")

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_hoursWorkedMainJob)

        # Recoding
        Data$hoursWorkedMainJob <- Data[[crosswalk_i$var_hoursWorkedMainJob]]
        Data[hoursWorkedMainJob == 99, hoursWorkedMainJob := NA] #certamente depois de 1981 é assim

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "occupationalStatus",
                                                         general_or_specific = "general")

        Data[is.na(occupationalStatus) | occupationalStatus == 0, hoursWorkedMainJob := NA]
        Data <- harmonizePNAD:::erase_just_created_vars(Data)


        gc()

        Data


}
