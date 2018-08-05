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

        check_Data_frame_convert_to_data_table(Data)
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


find_sulfix <- function(Data){

        metadata <- get_metadata(Data)

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

        sulfix
}

find_function <- function(Data, var_name){

        existing_functions <- unclass(lsf.str(envir = asNamespace("harmonizePNAD"), all = T))
        relevant_functions <- existing_functions[grep(pattern = var_name, x = existing_functions)]

        sulfix <- find_sulfix(Data)

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

        check_vars <- check_var_existence(Data, var_names)

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


build_onTheFly <- function(Data, var_name){

        if(!is.character(var_name) | length(var_name) != 1){
                stop("'var_name' must be a one-valued character vector")
        }

        check_var <- check_var_existence(Data, var_name)

        metadata = check_prepared_to_harmonize(Data)

        just_created_vars_list()

        if(length(check_vars) == 1) {

                f <- find_function(Data, var_name)

                call <- paste0(f,"(Data)")
                Data <- eval(parse(text = call))
                gc(); Sys.sleep(.5); gc()

                just_created_vars <<- c(just_created_vars, var_name)
        }

        Data
}










