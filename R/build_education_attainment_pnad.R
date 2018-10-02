build_education_attainment_pnad <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "specific")

        if(sulfix == "pnad1973"){
                warning("PNAD 1973: It is not possible to build a variable for education attainment for the year 1973.")
                return(Data)
        }

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "yearsSchooling",
                                                         general_or_specific = "specific")

        Data[ , attainment := cut(yearsSchooling,
                                  breaks = c(-1, 0, 3, 4, 7, 8, 10, 11, 14, 15),
                                  include.lowest = T,
                                  right = T)]

        Data[ , attainment := as.numeric(attainment)]

        gc(); Sys.sleep(.3); gc()

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        Data
}

