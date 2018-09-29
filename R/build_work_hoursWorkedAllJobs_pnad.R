build_work_hoursWorkedAllJobs_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_hoursWorked.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)
        #crosswalk   <- data.table::fread("E:/Google Drive/RCodes/PacotesR/harmonizePNAD/inst/extdata/crosswalk_pnad_hoursWorked.csv")

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]


        if(metadata$year %in% 1976:1990){
                # Checking the variable availability
                harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_hoursWorkedAllJobs)

                # Recoding
                Data$hoursWorkedMainJob <- Data[[crosswalk_i$var_hoursWorkedAllJobs]]
                Data[hoursWorkedMainJob == 99, hoursWorkedMainJob := NA] #certamente depois de 1981 é assim
        }

        if(metadata$year %in% 1992:2015){

                vars <- with(crosswalk_i, c(var_hoursWorkedMainJob,
                                            var_hoursWorkedSecondJob,
                                            var_hoursWorkedThirdJob))

                # Checking the variable availability
                harmonizePNAD:::check_necessary_vars(Data, vars)

                # Recoding
                matrix_hours <- as.matrix(Data[, vars, with = F])

                all_na       <- apply(matrix_hours, 1, function(x) sum(is.na(x)) == ncol(matrix_hours))

                missing_location = NULL
                for(j in 1:ncol(matrix_hours)){
                        missing_location_j <- which(matrix_hours[,j] == 99)

                        if(length(missing_location_j) > 0){
                                missing_location <- rbind(missing_location,
                                                          cbind(missing_location_j, j)
                                )
                        }
                }

                if(!is.null(missing_location)){
                        matrix_hours[missing_location] <- NA
                }


                Data[, hoursWorkedAllJobs := rowSums(matrix_hours, na.rm = T)]
                Data[all_na, hoursWorkedAllJobs := NA]
        }

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "occupationalStatus",
                                                         general_or_specific = "general")

        Data[is.na(occupationalStatus) | occupationalStatus == 0, hoursWorkedAllJobs := NA]
        Data <- harmonizePNAD:::erase_just_created_vars(Data)

        gc()

        Data
}
