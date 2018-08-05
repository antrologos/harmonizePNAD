check_Data_frame_convert_to_data_table <- function(Data){
        if(!is.data.frame(Data)){
                stop("'Data' is not a Data.frame")
        }

        if(!is.data.table(Data)){
                as.data.table(Data)
        }else{
                Data
        }
}


check_prepared_to_harmonize <- function(Data){
        test <- attributes(Data)$readyToHarmonize

        if(is.null(test)){
                stop("The data was not prepared to be harmonized. Use 'prepare_to_harmonize()' to make it ready to use with harmonizePNAD")
        }

        if(test == FALSE){
                stop("The data could not prepared to be harmonized. There was some kind of error when your ran 'prepare_to_harmonize()'")
        }

        harmonizePNAD:::check_Data_frame_convert_to_data_table(Data)
}


get_metadata <- function(Data){

        metadata = list(type = attributes(Data)$type,
                        year = attributes(Data)$year)

        if(metadata$type == "pnadc"){
                metadata$pnadc_freq <- attributes(Data)$pnadc_freq
                if(metadata$pnadc_freq == "quarterly"){
                        metadata$quarter <- attributes(Data)$quarter
                }
        }

        metadata
}


find_sulfix <- function(Data, general_or_specific){

        if(length(general_or_specific) != 1 | !(general_or_specific %in% c("general", "specific"))){

                stop("'general_or_specific' must be equal to 'general' or 'specific'")
        }

        metadata <- harmonizePNAD:::get_metadata(Data)

        if(general_or_specific == "general"){

                sulfix <- metadata$type

        }else{

                if(metadata$type == "pnad"){

                        if(metadata$year %in% 1973){
                                sulfix = "pnad1973"
                        }

                        if(metadata$year %in% 1976:1979){
                                sulfix = "pnad1970s"
                        }

                        if(metadata$year %in% 1981:1990){
                                sulfix = "pnad1980s"
                        }

                        if(metadata$year %in% 1992:2015){
                                sulfix = "pnad1990s"
                        }

                }

                if(metadata$type == "pnadc"){
                        sulfix = "pnadc"
                }

                if(metadata$type == "census"){
                        sulfix = paste0("census",metadata$year)
                }

        }

        sulfix
}

find_function <- function(Data, pattern, general_or_specific){

        existing_functions <- unclass(lsf.str(envir = asNamespace("harmonizePNAD"), all = T))
        relevant_functions <- existing_functions[grep(pattern = pattern, x = existing_functions)]

        sulfix <- harmonizePNAD:::find_sulfix(Data, general_or_specific = general_or_specific)

        f_parts <- strsplit(relevant_functions, split = "_")[[1]][1:3]
        f_parts <- c(f_parts, sulfix)

        f <- paste(f_parts, collapse = "_")

        f
}


check_var_existence <- function(Data, var_names){

        test <- sapply(var_names, function(x) is.null(Data[[x]]))

        problematic <- NULL
        problematic <- names(test)[which(test)]

        problematic
}


check_necessary_vars <-function(Data, var_names){

        check_vars <- harmonizePNAD:::check_var_existence(Data, var_names)

        if(length(check_vars) > 0){
                stop("The following variables are missing from the Data: ",
                     paste(check_vars, collapse = ", "))
        }
}


just_created_vars_list = function(){
        if(!exists("just_created_vars", envir = parent.frame())){
                just_created_vars <<- NULL
        }
}




build_onTheFly <- function(Data, var_name, general_or_specific){

        if(!is.character(var_name) | length(var_name) != 1){
                stop("'var_name' must be a one-valued character vector")
        }

        harmonizePNAD:::just_created_vars_list()

        f    <- harmonizePNAD:::find_function(Data, var_name, general_or_specific)

        call <- paste0(f,"(Data)")
        Data <- eval(parse(text = call))
        gc(); Sys.sleep(.5); gc()

        just_created_vars <<- c(just_created_vars, var_name)

        Data
}

check_and_build_onTheFly <- function(Data, var_name, general_or_specific){

        test <- harmonizePNAD:::check_var_existence(Data = Data, var_names = var_name)

        if(length(test) == 1){
                Data <- harmonizePNAD:::build_onTheFly(Data = Data,
                                                       var_name = var_name,
                                                       general_or_specific = general_or_specific)
        }

        Data
}





erase_just_created_vars <- function(Data){
        if(exists("just_created_vars", envir = parent.frame())){
                if(is.character(just_created_vars)){
                        just_created_vars <- just_created_vars[just_created_vars %in% names(Data)]
                        Data[ , c(just_created_vars) := NULL]
                }

                rm(just_created_vars, envir = .GlobalEnv)
        }
        Data
}

list_available_harmonizations <- function(x){
        objects <- ls("package:harmonizePNAD")
        objects <- objects[grep(x = objects, pattern = x)]
        objects <- objects[!(objects == paste0("harmonize_",x))]

        objects
}






