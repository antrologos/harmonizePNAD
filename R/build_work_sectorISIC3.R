#' @export

build_work_sectorISIC3 <- function(Data){

        Data     <- harmonizePNAD:::check_prepared_to_harmonize(Data)
        metadata <- harmonizePNAD:::get_metadata(Data)
        sulfix   <- harmonizePNAD:::find_sulfixforOccSectors(Data)

        crosswalk_location <- system.file("extdata",
                                     "crosswalk_sector_isic3.xlsx",
                                     package = "harmonizePNAD")

        varList_location <- system.file("extdata",
                                          "varList_sector.csv",
                                          package = "harmonizePNAD")

        crosswalk <- readxl::read_excel(crosswalk_location) %>%
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

        Data[ , sector_code := Data[[varName]] ]

        Data = crosswalk[Data, on = "sector_code"]
        gc(); Sys.sleep(.3);gc()

        Data <- Data %>%
                select(-sectorISIC3, everything(), sectorISIC3, -sector_code)

        Data[sectorISIC3 == 0, sectorISIC3 := NA]
        gc()

        Data
}

