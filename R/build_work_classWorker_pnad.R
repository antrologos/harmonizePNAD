build_work_classWorker_pnad <- function(Data){

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_classWorker.csv",
                                     package = "harmonizePNAD")
        crosswalk   <- data.table::fread(file_location)

        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizePNAD:::get_metadata(Data)
        crosswalk_i <- crosswalk[year == metadata$year]

        # Checking the variable availability
        harmonizePNAD:::check_necessary_vars(Data, crosswalk_i$var_classWorker)

        # Recoding
        Data[ , classWorker := as.numeric(NA)]

        expr_employee   <- with(crosswalk_i, paste(var_classWorker,"%in% c(",classWorker_employee, ")"))
        expr_employer   <- with(crosswalk_i, paste(var_classWorker,"%in% c(",classWorker_employer, ")"))
        expr_selfEmpl   <- with(crosswalk_i, paste(var_classWorker,"%in% c(",classWorker_selfEmpl, ")"))
        expr_unpaid     <- with(crosswalk_i, paste(var_classWorker,"%in% c(",classWorker_unpaid, ")"))

        Data[eval(parse(text = expr_employee)),    classWorker := 1]
        Data[eval(parse(text = expr_employer)),    classWorker := 2]
        Data[eval(parse(text = expr_selfEmpl)),    classWorker := 3]
        Data[eval(parse(text = expr_unpaid)),      classWorker := 5]

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "occupationalStatus",
                                                         general_or_specific = "general")
        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "econActivity",
                                                         general_or_specific = "general")

        Data[is.na(occupationalStatus) | occupationalStatus == 0, classWorker := NA]
        Data[is.na(econActivity)       | econActivity == 0      , classWorker := NA]
        Data <- harmonizePNAD:::erase_just_created_vars(Data)


        gc()

        Data


}
