
adjust_work_hoursWorkedMainJob_pnad = function(Data){

        Data <- harmonizePNAD:::check_prepared_to_harmonize(Data)

        if(!exists("hoursWorkedMainJob",Data)){
                warning("The variable 'hoursWorkedMainJob' is not in your data")
                return(Data)
        }

        if(exists("classWorker",Data)){
                data.table::setnames(x = Data, old = "classWorker", new = "classWorker_tmp")
                classWorker_justCreated <- FALSE
        }else{
                classWorker_justCreated <- TRUE
        }


        Data <- harmonizePNAD:::build_work_classWorker(Data, mergeRuralClasses = F)

        if(exists("occupationalStatus",Data)){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, occupationalStatus := NA]
        }

        if(exists("employRecordBook",Data)){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, employRecordBook := NA]
        }

        if(exists("socialSecurityContrib",Data)){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, socialSecurityContrib := NA]
        }

        if(exists("sectorISIC3",Data)){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, sectorISIC3 := NA]
        }

        if(exists("econActivity",Data)){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, econActivity := 0]
        }

        if(exists("hoursWorkedAllJobs",Data)){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, hoursWorkedAllJobs := NA]
        }

        if(classWorker_justCreated ==  FALSE){
                Data[hoursWorkedMainJob < 15 & classWorker == 6, classWorker_tmp  := NA]
        }

        Data[hoursWorkedMainJob < 15 & classWorker == 6, hoursWorkedMainJob := NA]

        if(classWorker_justCreated ==  FALSE){
                Data[, classWorker := NULL]
                data.table::setnames(Data, old = "classWorker_tmp", new = "classWorker")
        }

        gc(); Sys.sleep(.2); gc()
        Data
}



