rm(list = ls());gc()
options(scipen = 999)
library(data.table)
library(tidyverse)
library(harmonizePNAD)
library(Hmisc)
setwd("C:/Dropbox/Rogerio/Bancos_Dados/PNADs/")

anos <- c(1973, 1976:1979,1981, 1990,1992,2006,2015)

# Abrindo arquivos
for(ano_i in anos){
        print(ano_i)
        arquivo = paste0("PNAD ", ano_i,"/pnad.pes_", ano_i,".csv")
        assign(x = paste0("p_", ano_i),
               value = fread(arquivo) %>% prepare_to_harmonize(type = "pnad", year = ano_i))
}

# Harmonizacoes
for(ano_i in anos){
        print(ano_i)
        assign(x = paste0("p_", ano_i),
               value = get(paste0(paste0("p_", ano_i))) %>%
                       build_education_yearsSchooling() %>%
                       build_identification_wgt() %>%
                       build_demographics_age())
}

# Estatísticas
map_df(.x = anos,
       .f = function(ano_i){
               age_i = get(paste0("p_", ano_i), envir = .GlobalEnv)[, wtd.mean(x = age, weights = fweight)]
               if(ano_i == 1973){
                       yearsSchooling_i = NA
               }else{
                       yearsSchooling_i = get(paste0("p_", ano_i), envir = .GlobalEnv)[, wtd.mean(x = yearsSchooling, weights = fweight)]
               }
               tibble(year           = ano_i,
                      age            = age_i,
                      yearsSchooling = yearsSchooling_i)
       }
)

