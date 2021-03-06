#' @export

build_work_sectorISIC3 <- function(Data){

        Data     <- harmonizePNAD:::check_prepared_to_harmonize(Data)
        metadata <- harmonizePNAD:::get_metadata(Data)
        sulfix   <- harmonizePNAD:::find_sulfixforOccSectors(Data)

        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)

        crosswalk_location <- system.file("extdata",
                                     "crosswalk_sector_isic3.csv",
                                     package = "harmonizePNAD")

        varList_location <- system.file("extdata",
                                          "varList_sector.csv",
                                          package = "harmonizePNAD")

        crosswalk <- read.csv2(crosswalk_location, stringsAsFactors = F) %>%
                filter(classification == sulfix) %>%
                select(sector_code, sectorISIC3) %>%
                as.data.table()

        varList   <- read.csv2(varList_location, stringsAsFactors = F)

        if(metadata$type != "pnadc"){
                varName <- varList %>%
                        filter(data == metadata$type & year == metadata$year) %>%
                        .$var_sector
        }else{
                varName <- varList %>%
                        filter(data == metadata$type) %>%
                        .$var_sector
        }

        harmonizePNAD:::check_necessary_vars(Data, varName)

        Data[ , sector_code := Data[[varName]] ]

        # efficient join using data.table sintax:
        Data = crosswalk[Data, on = "sector_code"] # this causes loss of the metadata

        Data = harmonizePNAD:::set_metadata(Data, metadata) #recovering metadata...

        gc(); Sys.sleep(.3);gc()

        Data <- Data %>%
                select(-sectorISIC3, everything(), sectorISIC3, -sector_code)

        Data[sectorISIC3 == 0, sectorISIC3 := NA]

        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "occupationalStatus",
                                                         general_or_specific = "general")
        Data <- harmonizePNAD:::check_and_build_onTheFly(Data,
                                                         var_name = "econActivity",
                                                         general_or_specific = "general")

        Data[is.na(occupationalStatus) | occupationalStatus == 0, sectorISIC3 := NA]
        Data[is.na(econActivity)       | econActivity == 0      , sectorISIC3 := NA]

        Data[ occupationalStatus == 1 & is.na(sectorISIC3), sectorISIC3 := 999]

        if(just_created_vars_list_existedBefore == F){
                Data <- harmonizePNAD:::erase_just_created_vars(Data)
        }

        gc()

        Data
}

