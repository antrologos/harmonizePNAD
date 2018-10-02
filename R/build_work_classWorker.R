#' Builds a synthetic variable for education attainment
#' @param data.frame
#' @value data.frame
#' @export

build_work_classWorker <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)
        Data <- harmonizePNAD:::check_and_build_onTheFly(Data, "sectorISIC3", general_or_specific = NA)

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = "general")
        call   <- paste0("harmonizePNAD:::build_work_classWorker_", sulfix, "(Data)")
        Data   <- eval(parse(text = call))

        gc(); Sys.sleep(.3); gc()

        # Rural self-employed and unpaid workers will be classified into a separated category
        Data[sectorISIC3 == 10 & classWorker == 3, classWorker := 4] # self-employed
        Data[sectorISIC3 == 10 & classWorker == 5, classWorker := 4] # unpaid

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        gc()

        Data

}
