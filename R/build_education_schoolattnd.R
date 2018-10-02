#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_education_schoolattnd <- function(Data, ajust_for_age = T){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_education_schoolattnd_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))
        gc()

        if(ajust_for_age == T){
                Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                                 var_name = "age",
                                                                 general_or_specific = "general")

                Data[age < 5 , schoolattnd := NA]

                if(just_created_vars_list_existedBefore == F){
                        Data <- harmonizePNAD:::erase_just_created_vars(Data)
                }
        }

        gc(); Sys.sleep(.3); gc()

        Data
}

