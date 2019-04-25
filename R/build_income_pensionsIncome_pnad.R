build_income_pensionsIncome_pnad <- function(Data){

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        metadata = harmonizePNAD:::get_metadata(Data)

        # Loading the crosswalk
        file_location <- system.file("extdata",
                                     "crosswalk_pnad_pensionsIncome.csv",
                                     package = "harmonizePNAD")


        #file_location <-  "C:\\Users\\Rogerio\\Google Drive\\PacotesR\\harmonizePNAD\\inst\\extdata\\crosswalk_pnad_pensionsIncome.csv"
        crosswalk <- data.table::fread(file_location, colClasses = "numeric", dec =",")
        crosswalk <- crosswalk[year == metadata$year]

        income_vars <- str_split(crosswalk$var_pensionsIncome,pattern = ";") %>% unlist()
        harmonizePNAD:::check_necessary_vars(Data = Data, var_names = income_vars)

        sub_data = Data[, .SD, .SDcols = income_vars] %>% as.data.frame()

        sub_data[sub_data >= crosswalk$missing_values] <- NA

        all_NA_tmp = apply(sub_data, 1, function(x) sum(is.na(x)) == ncol(sub_data))

        sub_data[is.na(sub_data)] <- 0

        Data[, pensionsIncome := rowSums(sub_data)]
        Data[all_NA_tmp == T, pensionsIncome := NA]

        gc()
        Data
}

