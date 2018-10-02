
build_education_attainment1973standard_pnad <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "specific")

        if(sulfix == "pnad1973"){
                harmonizePNAD:::check_necessary_vars(Data, "v0158")

                Data[ , attainment1973standard := as.numeric(NA)]

                Data[ v0158 == 0 , attainment1973standard := 1]
                Data[ v0158 == 2 , attainment1973standard := 3]
                Data[ v0158 == 3 , attainment1973standard := 5]
                Data[ v0158 == 4 , attainment1973standard := 4]
                Data[ v0158 == 5 , attainment1973standard := 7]
                Data[ v0158 == 6 , attainment1973standard := 6]
                Data[ v0158 == 7 , attainment1973standard := 9]
                Data[ v0158 == 8 , attainment1973standard := 8]
                Data[ v0158 == 9 , attainment1973standard := NA]

        }else{
                Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                                 var_name = "attainment",
                                                                 general_or_specific = "general")
                gc()

                Data[  , attainment1973standard := attainment]
                Data[attainment == 2 , attainment1973standard := 1]

                if(just_created_vars_list_existedBefore == F){
                        Data <- harmonizePNAD:::erase_just_created_vars(Data)
                }
        }


        Data
}



