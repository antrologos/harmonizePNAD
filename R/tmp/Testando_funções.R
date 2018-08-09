rm(list = ls());gc()
options(scipen = 999)
library(data.table)
library(tidyverse)
library(harmonizePNAD)
library(Hmisc)
setwd("C:/Dropbox/Rogerio/Bancos_Dados/PNADs/")

anos <- c(1973, 1976:1979,1981:1990,1992, 1993, 1995:1999, 2001:2009, 2011:2015)

ano_i = 2015
# Abrindo arquivos
for(ano_i in c(2001:2009,2011:2015)){
        print(ano_i)
        arquivo = paste0("PNAD ", ano_i,"/pnad.pes_", ano_i,".csv")
        assign(x = paste0("p_", ano_i),
               value = fread(arquivo) %>% prepare_to_harmonize(type = "pnad", year = ano_i))
        gc()
}

# Harmonizacoes - testes de variaveis especificas
for(ano_i in c(2001:2009,2011:2015)){
        print(ano_i)
        assign(x = paste0("p_", ano_i),
               value = get(paste0(paste0("p_", ano_i))) %>%
                       build_identification_year() %>%
                       build_identification_wgt() %>%
                       build_work_occupationalStatus()
        )
        gc();Sys.sleep(.3);gc()
}

gc()
dados_existentes <- ls()[grep(x = ls(), pattern="p_[[:digit:]]{4}")]
dados_stack = data_frame()
for(dado_i in dados_existentes){
        print(dado_i)
        dados_stack <- bind_rows(dados_stack,
                                 get(dado_i) %>%
                                         select(year, occupationalStatus, fweight) %>%
                                         filter(complete.cases(.))
                                 )
}
dados_stack <- data.table(dados_stack)

freq = dados_stack[ , questionr::wtd.table(x = occupationalStatus,
                                           y = year,
                                           weights = fweight)]
round(prop.table(freq,margin = 2)*100, 2)






# Harmonizacoes completas
for(ano_i in anos){
        print(ano_i)
        assign(x = paste0("p_", ano_i),
               value = get(paste0(paste0("p_", ano_i))) %>%
                       harmonize_identification() %>%
                       harmonize_demographics() %>%
                       harmonize_geography() %>%
                       harmonize_education() %>%
                       harmonize_work()
                       )
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


map(.x = anos,
    .f = function(ano_i){
            freq_uf <- get(paste0("p_", ano_i), envir = .GlobalEnv)[, table(x = regionMCA)]
            dados   <- data.table(as.numeric(freq_uf))
            rownames(dados) <- names(freq_uf)
            names(dados)    <- paste0("ano",ano_i)
            dados
    }) %>%
        reduce(.f = bind_cols)


vars_to_harmonize_ordered_list <- vars_to_harmonize()
write.csv2(vars_to_harmonize_ordered_list,
           file = "C:/Users/Rogerio/Google Drive/RCodes/PacotesR/harmonizePNAD/inst/extdata/vars_to_harmonize_ordered_list.csv",
           row.names = F)
