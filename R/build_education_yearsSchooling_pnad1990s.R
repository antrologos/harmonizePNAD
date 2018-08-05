

build_education_yearsSchooling_pnad1990s <- function(Data){

        metadata = get_metadata(Data)
        necessary_var = ifelse(metadata$year >= 2007, "v4803", "v4703")
        check_necessary_vars(Data = Data, necessary_var)

        Data$yearsSchooling = Data[[necessary_var]]

        Data[ , yearsSchooling := yearsSchooling - 1]
        Data[yearsSchooling == 16, yearsSchooling := NA]

        Data
}

