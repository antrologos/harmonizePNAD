build_education_attainment_pnad <- function(Data){

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "yearsSchooling",
                                                         general_or_specific = "specific")

        Data[ , attainment := cut(yearsSchooling,
                                  breaks = c(-1, 0, 3, 4, 7, 8, 10, 11, 14, 15),
                                  include.lowest = T,
                                  right = T)]

        Data[ , attainment := as.numeric(attainment)]

        gc(); Sys.sleep(.3); gc()

        Data <- harmonizePNAD:::erase_just_created_vars(Data)
        Data
}

