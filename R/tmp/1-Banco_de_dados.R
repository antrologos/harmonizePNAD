rm(list=ls())
gc()   
# Definindo diretorios de trabalho
#dir.pnads   <- "c:/users/rogerio/Dropbox/bancos_dados/pnads/"
#dir.artigo  <- "c:/users/rogerio/Dropbox/formacao/doutorado/tese/banco_dados/"
dir.pnads  <- "E:/Dropbox-Ro/Dropbox/Rogerio/bancos_dados/pnads/"
dir.artigo <- "E:/Dropbox-Ro/Dropbox/Rogerio/formacao/doutorado/tese/banco_dados/"

#install.packages(c("bit64","data.table","IC2","Hmisc","MASS","plyr","sandwich"), dep=T)
#rm(list=ls());gc()
########################################################
# Carregando pacotes e definindo opcoes
options(scipen=999,
        stringsAsFactors=FALSE)
library(bit64)      # para que o data.table possa comportar integers de 64 bits
library(data.table) # data.frames mais eficientes...
library(Hmisc)      # estatisticas descritivas com ponderacao
library(plyr)       # para usar a funcao rename


########################################################
# Carrega a funcao que padroniza os anos de estudo para as 
# pnads dos anos 1980

setwd(dir.artigo)
source("1b-Funcao_corrige_educ_1980s.R")
source("1c-Funcao_corrige_educ_1976_9.R")
source("1d_Funcao_padronizaUF.R")
########################################################
# Inicio da analise - configuracoes preliminares
setwd(dir.pnads)

# Lista as PNADs existentes 
# Cada PNAD deve estar num diretorio separado, dentro do diretorio
# principal. O nome de cada um desses subdiretorios deve ser como
# o seguinte exemplo: "PNAD 2012"
diretorios <- list.files()
diretorios <- diretorios[-grep("[.]|[-]",diretorios)]
diretorios <- diretorios[grep("PNAD ",diretorios)]

# Extrai o ano da pesquisa, a partir do nome do diretorio
anos <- gsub("PNAD ","" ,diretorios)
anos = as.numeric(anos)

diretorios = diretorios[anos>=1976]
anos = anos[anos>=1976]  

# Carrega uma planilha CSV que contem o nome das variaveis de interesse 
# tal como presente em cada um dos anos de interesse. Nessa planilha há 
# também informacoes sobre a codificacao das variaveis e indices para 
# deflacionar as variaveis de renda
variaveis <- read.csv2(paste0(dir.artigo,"variaveis_utilizadas.csv"),
                       header=TRUE, dec=",")



# Cria um objeto que recebera as informacoes de todos os anos da pesquisa
# Ou seja, esse sera um grande banco de dados, empilhando todos os anos 
# selecionados
banco <- NULL


# Cria uma lista que contem o nome de todos os objetos que NAO devem ser 
# deletados a cada iteracao do loop abaixo
objetos_permanentes <- ls()
objetos_permanentes <- c(objetos_permanentes,"objetos_permanentes","i")

########################################################
# Abrindo os bancos de dados

# Loop para abrir as bases de dados de todos os anos, aplicar transformacoes e
# recodificacoes, e empilhar os dados extraidos de cada ano com os demais

#i = 11
for(i in 1:length(diretorios)){
        
        gc() # limpando a memoria 
        
        # Indica o ano que será aberto na iteracao
        diretorio = diretorios[i]
        ano = anos[i]
        
        cat("Abrindo PNAD", ano, "\n")
        
        # Obtem o nome do arquivo a ser aberto
        cat("---- Formatando nome de arquivo\n")
        
        arquivo_pes <- paste0("pnad.pes_",ano,".csv")
        if(ano != 1977) 
                arquivo_dom <- paste0("pnad.dom_",ano,".csv")
        
        #######################
        # Abrindo o banco
        
        # Selecionando as variaveis
        cat("---- Abrindo apenas 10 casos...\n")
        tmp <- read.table(paste0(diretorio,"/",arquivo_pes),
                          sep = "\t",
                          header = T,
                          dec = ".",
                          nrows = 10,
                          comment.char="",
                          stringsAsFactors= FALSE)
        
        cat("---- Verificando o nome das variaveis no ano de", ano, "\n")
        variaveis_ano  <- variaveis[variaveis$ano == as.numeric(ano),]
        variaveis_nome <- as.character(variaveis_ano)
        variaveis_nome <- variaveis_nome[grep("[[:alpha:]]",variaveis_nome)]
        
        variaveis_nome <- tolower(variaveis_nome)
        variaveis_banco <- tolower(names(tmp))
        
        variaveis_selecionadas <- variaveis_banco %in% variaveis_nome
        
        classes_colunas <- rep("NULL",length(variaveis_banco))
        classes_colunas[variaveis_selecionadas] <- "numeric"
        
        cat("---- Abrindo apenas as variaveis selecionadas \n")
        tmp <- fread(paste0(diretorio,"/",arquivo_pes),
                     colClasses = classes_colunas)
        
        
        if(ano != 1977){
                cat("---- Abrindo apenas 10 casos de domicilios...\n")
                tmp_dom <- read.table(paste0(diretorio,"/",arquivo_dom),
                                      sep = "\t",
                                      header = T,
                                      dec = ".",
                                      nrows = 10,
                                      comment.char="",
                                      stringsAsFactors= FALSE)
                
                variaveis_banco <- tolower(names(tmp_dom))
                variaveis_nome_dom <- c("domicilioid", 
                                        variaveis_ano$npessoas, 
                                        variaveis_ano$rendadomiciliar,
                                        variaveis_ano$rural_urbano)
                
                variaveis_selecionadas <- variaveis_banco %in% variaveis_nome_dom
                classes_colunas <- rep("NULL",length(variaveis_banco))
                classes_colunas[variaveis_selecionadas] <- "numeric"
                
                tmp_dom <- fread(paste0(diretorio,"/",arquivo_dom),
                                 colClasses = classes_colunas)
                
                num_repetida <- setdiff(
                        # identificando as variaveis que se repetem nos dois bancos (dom e pessoas)
                        which( names(tmp_dom) %in% names(tmp) ), 
                        
                        # identificando qual dessas eh a domicilioid
                        grep("domicilioid", names(tmp_dom)) 
                        
                )
                if(length(num_repetida) > 0 ){
                        tmp_dom <- tmp_dom[, -num_repetida, with = F]
                }
                
                if(ncol(tmp_dom)<=1){
                        rm(tmp_dom);gc()
                }else{
                        setkey(tmp, "domicilioid")
                        setkey(tmp_dom, "domicilioid")
                        tmp <- merge(tmp, tmp_dom)
                        rm(tmp_dom);gc()
                }
                
        }
        
        if( ano <= 1979){
                cat("-------- condicao de presenca: excluindo nao moradores \n")
                tmp$condicao_moradores = tmp[[variaveis_ano$cond_presenca]]
                tmp = tmp[condicao_moradores != variaveis_ano$nao_morador]
        }
        
        #################################
        # Recodificacoes
        
        cat("---- Iniciando as recodificacoes \n")
        setnames(tmp, names(tmp), tolower(names(tmp)))
        
        cat("-------- sexo \n")
        tmp$sexo       <- ifelse(tmp[[variaveis_ano$sexo]] == variaveis_ano$sexo_h, 1, 0)
        
        cat("-------- idade \n")
        setnames(tmp, variaveis_ano$idade, "idade")
        tmp[idade > 150, idade := NA]
        
        cat("-------- peso \n")
        setnames(tmp, variaveis_ano$peso, "peso")
        
        cat("-------- UF \n")
        setnames(tmp, variaveis_ano$uf, "uf")
        tmp[ , uf_padr := padronizaUF(uf, ano=ano)]
        
        
        cat("-------- raca \n")
        tmp$ind_brancos_amarelos  = as.numeric(NA)
        tmp$ind_pretos            = as.numeric(NA)
        tmp$ind_pardos_indig_outr = as.numeric(NA)
        
        if(ano >= 1987){
                valor_brancos_amarelos <- as.numeric( strsplit(variaveis_ano$valor_brancos_amarelos, ";")[[1]])
                valor_pretos           <- as.numeric( variaveis_ano$valor_pretos )
                valor_pardos_outros    <- as.numeric( strsplit(variaveis_ano$valor_pardos_outros, ";")[[1]])
        
                tmp$ind_brancos_amarelos  <- as.numeric( tmp[[variaveis_ano$raca]] %in% valor_brancos_amarelos )
                tmp$ind_pretos            <- as.numeric( tmp[[variaveis_ano$raca]] %in% valor_pretos )
                tmp$ind_pardos_indig_outr <- as.numeric( tmp[[variaveis_ano$raca]] %in% valor_pardos_outros )
                
                tmp$ind_brancos_amarelos[is.na(tmp[[variaveis_ano$raca]])]  <- NA
                tmp$ind_pretos[is.na(tmp[[variaveis_ano$raca]])]            <- NA
                tmp$ind_pardos_indig_outr[is.na(tmp[[variaveis_ano$raca]])] <- NA
        }
        
        cat("-------- rural/urbano \n")
        valor_rural <- as.numeric( strsplit(variaveis_ano$valor_rural, ";")[[1]])
        tmp$rural <- as.numeric( tmp[[variaveis_ano$rural_urbano]] %in% valor_rural )
        tmp$rural_urbano[is.na(tmp[[variaveis_ano$rural_urbano]])] <- NA
        
        cat("-------- Ano \n")
        tmp$ano <- as.numeric(ano)
        
        
        cat("-------- posicao no domicilio: chefe e conjuge \n")
        tmp$chefe_dom     = ifelse(tmp[[variaveis_ano$pos_dom]]==1,1,0)
        tmp$conjuge_dom   = ifelse(tmp[[variaveis_ano$pos_dom]]==2,1,0)
        
        
        cat("-------- posicao no domicilio: parentes e nao parentes \n")
        tmp$ind_familia_principal = ifelse(tmp[[variaveis_ano$pos_dom]]<= variaveis_ano$parente_threshold, 1, 0)
        tmp[ , npessoas_fam_principal := sum(ind_familia_principal), by=domicilioid]
        
        
        cat("-------- anos de estudo\n")
        if(as.numeric(ano) %in% 1976:1979){
                tmp$anosest <- corrige_educ_1976_9(tmp, ano)
        }
        
        if(as.numeric(ano) %in% 1981:1990){
                tmp$anosest <- corrige_educ_1980s(tmp)
        }
        
        if(as.numeric(ano) >= 1992){
                tmp$anosest <- tmp[[variaveis_ano$anosest]] - 1 
                tmp[anosest >= 16, anosest := NA]
        }
        
        tmp[anosest < 0, anosest := NA]
        
        cat("-------- PEA\n")
        valores_pea <- as.numeric( strsplit(variaveis_ano$pea_valores, ";")[[1]])
        tmp$pea <- tmp[[variaveis_ano$pea]] %in% valores_pea
        tmp$pea[is.na(tmp[[variaveis_ano$pea]])] <- NA
        tmp$pea[tmp$idade < 10] <- NA
        tmp$pea = as.numeric(tmp$pea)
        
        
        cat("-------- Condicao de ocupacao\n")
        valores_ocup <- as.numeric( strsplit(variaveis_ano$ocup_valores, ";")[[1]])
        tmp$cond_ocup <- tmp[[variaveis_ano$cond_ocup]] %in% valores_ocup
        tmp$cond_ocup[is.na(tmp[[variaveis_ano$cond_ocup]])] <- NA
        tmp$cond_ocup[tmp$idade < 10] <- NA
        tmp$cond_ocup[tmp$pea == 0]   <- NA
        tmp$cond_ocup = as.numeric(tmp$cond_ocup)
        
        
        cat("-------- Posicao na ocupacao\n")
        valores_empregador <- as.numeric( strsplit(variaveis_ano$pos_empregador, ";")[[1]])
        valores_empregado  <- as.numeric( strsplit(variaveis_ano$pos_empregado, ";")[[1]])
        valores_conta_prop <- as.numeric( strsplit(variaveis_ano$pos_contapropria, ";")[[1]])
        
        tmp$ind_empregador <- as.numeric(tmp[[variaveis_ano$pos_ocup]] %in% valores_empregador)
        tmp$ind_empregado  <- as.numeric(tmp[[variaveis_ano$pos_ocup]] %in% valores_empregado)
        tmp$ind_conta_prop <- as.numeric(tmp[[variaveis_ano$pos_ocup]] %in% valores_conta_prop)
        
        tmp[idade < 10 | is.na(cond_ocup) | cond_ocup == 0, ind_empregador := NA]
        tmp[idade < 10 | is.na(cond_ocup) | cond_ocup == 0, ind_empregado  := NA] 
        tmp[idade < 10 | is.na(cond_ocup) | cond_ocup == 0, ind_conta_prop := NA]
        
        
        cat("-------- carteira de trabalho e formalizacao\n")
        valor_carteira <- as.numeric( strsplit(variaveis_ano$valor_carteira, ";")[[1]])
        tmp$carteira   <- as.numeric(tmp[[variaveis_ano$carteira_assinada]] %in% valor_carteira)
        tmp[ is.na(cond_ocup) | cond_ocup == 0, carteira := NA]
        
        
        
        if(ano >= 1992){
                valor_estaturio_militar <- as.numeric( strsplit(variaveis_ano$valor_estaturio_militar, ";")[[1]])
                tmp$estaturio_militar   <- as.numeric(tmp[[variaveis_ano$carteira_assinada]] %in% valor_estaturio_militar)
                
                tmp[ is.na(cond_ocup) | cond_ocup == 0, estaturio_militar := NA]
        }else{
                tmp$estaturio_militar = as.numeric(NA)
        }
        
        
        
        cat("-------- horas trabalhadas: trabalho principal da semana\n")
        setnames(tmp, variaveis_ano$horas_trabprinc, "horas_trabprinc")
        tmp[horas_trabprinc > 98             , horas_trabprinc := NA]
        tmp[is.na(cond_ocup) | cond_ocup == 0, horas_trabprinc := NA]
        
        
        cat("-------- horas trabalhadas: todos os trabalhos da semana\n")
        if(ano <= 1990){
                setnames(tmp, variaveis_ano$horas_todostrab, "horas_todostrab")
                tmp[horas_todostrab > 98             , horas_todostrab := NA]
                tmp[is.na(cond_ocup) | cond_ocup == 0, horas_todostrab := NA]
        }
        
        if(ano >= 1992){
                setnames(tmp, 
                         c(variaveis_ano$horas_trabsec,variaveis_ano$horas_trabout),
                         c("horas_trabsec","horas_trabout")
                         )
                tmp[ , horas_trabsec := as.numeric(horas_trabsec)] # a PNAD de 2013 tem erros
                tmp[ , horas_todostrab := rowSums(cbind(horas_trabprinc, horas_trabsec, horas_trabout),na.rm=T)]
                tmp[horas_todostrab > 98             , horas_todostrab := NA]
                tmp[is.na(cond_ocup) | cond_ocup == 0, horas_todostrab := NA]
        }
        
        
        
        
        cat("-------- Ocupacao\n")
        setnames(tmp, variaveis_ano$ocup, "ocup_original")
        tmp[cond_ocup == 0, ocup_original := NA]
        
        
        cat("-------- Setor de atividade\n")
        setnames(tmp, variaveis_ano$setor, "setor_original")
        tmp[cond_ocup == 0, setor_original := NA]
        
        
        
        cat("-------- Coberturas amostrais compativeis \n")
        tmp$amostra_1976 = 1
        tmp$amostra_1981 = 1
        tmp$amostra_2004 = 1
        
        # Se parte da regiao norte ou centro-oeste (exceto Brasilia) + rural = nao faz parte do desenho vigente em 1976 
        tmp[( trunc(uf_padr/10) ==  1 | uf_padr %in% c(51,52) ) & rural == 1, amostra_1976 := 0]
        
        # Se parte da regiao norte (exceto tocantins) + rural = nao faz parte do desenho vigente em 1981 
        tmp[( trunc(uf_padr/10) ==  1) & rural == 1, amostra_1981 := 0]
        
        # Se anterior a 1981, nao faz parte do desenho de 1981
        tmp[ano <= 1979, amostra_1981 := 0]
        
        # Se anterior a 2004, nao faz parte do desenho de 2004
        tmp[ano <= 2003, amostra_2004 := 0]
        
        
        
        
        vars_renda = names(variaveis)[grep("renda",names(variaveis))]
        vars_renda = vars_renda[which(variaveis_ano[, vars_renda] != "")]
        
        
        ### RENDAS
        for(v in vars_renda){ 
                setnames(tmp, variaveis_ano[[v]], v)
                try({tmp[tmp[[v]] >= variaveis_ano$missing][[v]] = as.numeric(NA)}, silent=T)
                tmp[[v]] = tmp[[v]]*variaveis_ano$deflator
        } 
        
        
        
        
        if(ano == 1978){ # A PNAD de 1978 nao traz a renda de todos os trabalhos previamente calculada. 
                tmp$nao_miss = tmp[ ,(!is.na(rendatrab))|(!is.na(rendatrab_2))|(!is.na(rendatrab_3)) ]
                tmp[is.na(rendatrab), rendatrab := 0]
                tmp[is.na(rendatrab_2), rendatrab_2 := 0]
                tmp[is.na(rendatrab_3), rendatrab_3 := 0]
                tmp[ , rendatrab := rendatrab+rendatrab_2+rendatrab_3]
                
                
                
                tmp$nao_miss2 = tmp[ ,(!is.na(rendatrab))|
                                             (!is.na(renda_out_trab_1))|(!is.na(renda_out_trab_2))|(!is.na(renda_out_trab_3))|
                                             (!is.na(renda_out_ocup_1))|(!is.na(renda_out_ocup_2))|(!is.na(renda_out_ocup_3))
                                     ]
                
                tmp[is.na(renda_out_trab_1), renda_out_trab_1 := 0]
                tmp[is.na(renda_out_trab_2), renda_out_trab_2 := 0]
                tmp[is.na(renda_out_trab_3), renda_out_trab_3 := 0]
                tmp[is.na(renda_out_ocup_1), renda_out_ocup_1 := 0]
                tmp[is.na(renda_out_ocup_2), renda_out_ocup_2 := 0]
                tmp[is.na(renda_out_ocup_3), renda_out_ocup_3 := 0]
                
                tmp[ , rendatodostrab := rendatrab+
                             renda_out_trab_1+renda_out_trab_2+renda_out_trab_3+
                             renda_out_ocup_1+renda_out_ocup_2+renda_out_ocup_3
                     ]
                
                
                tmp[nao_miss==F, rendatrab := NA]
                tmp[nao_miss2==F, rendatodostrab := NA]
        }
        
        tmp[pea==0, rendatrab := NA]
        tmp[pea==0, rendatodostrab := NA]
        
        tmp[cond_ocup==0, rendatrab := NA]
        tmp[cond_ocup==0, rendatodostrab := NA]
        
        
        
        
        # criando aposentadorias, pensoes e outras comparaveis
        
        if(ano==1976){
                setnames(tmp, "renda_aposentadoria_pensao", "renda_todas_aposentadoria_e_pensao")
                tmp[is.na(renda_todas_aposentadoria_e_pensao), renda_todas_aposentadoria_e_pensao := 0 ]
                tmp[, renda_todas_outras := rowSums(cbind(renda_doacao,renda_outras), na.rm=T)]
        }
        
        if(ano==1977){
                tmp[, renda_todas_aposentadoria_e_pensao :=  rowSums(cbind(renda_aposentadoria1,renda_pensao1),na.rm=T)]
                tmp[, renda_todas_outras                 :=  rowSums(cbind(renda_doacao,renda_juros_outros,
                                                                           renda_venda_imoveis,renda_outras),na.rm=T)]
        }
        
        if(ano==1978){
                tmp[, renda_todas_aposentadoria_e_pensao :=  rowSums(cbind(renda_aposentadoria1,renda_pensao1),na.rm=T)]
                tmp[, renda_todas_outras :=  rowSums(cbind(renda_doacao,renda_outras),na.rm=T)]
        }
        
        if(ano==1979){
                setnames(tmp, "renda_aposentadoria_pensao", "renda_todas_aposentadoria_e_pensao")
                tmp[is.na(renda_todas_aposentadoria_e_pensao), renda_todas_aposentadoria_e_pensao := 0 ]
                tmp[, renda_todas_outras := rowSums(cbind(renda_doacao,renda_juros_outros,renda_outras),na.rm=T)]
        }
        
        if(ano %in% 1981:1990) {
                tmp[, renda_todas_aposentadoria_e_pensao :=  rowSums(cbind(renda_aposentadoria1,renda_pensao1),na.rm=T)]
                tmp[, renda_todas_outras :=  rowSums(cbind(renda_abono,renda_outras),na.rm=T)]
        }
        
        if(ano %in% 1992:2015) {
                tmp[, renda_todas_aposentadoria_e_pensao :=  rowSums(cbind(renda_aposentadoria1,renda_aposentadoria2,
                                                                           renda_pensao1,renda_pensao2),na.rm=T)] 
                
                tmp[, renda_todas_outras :=  rowSums(cbind(renda_doacao,renda_juros_outros,renda_abono),na.rm=T)] 
        }
        
        
        
        tmp[, rendaindividual := rowSums(cbind(rendatodostrab,renda_todas_aposentadoria_e_pensao,renda_aluguel,renda_todas_outras),
                                         na.rm=T)]
        
        
        # Calculando as rendas individuais, per capita
        tmp[, rendadomiciliar        := sum(ind_familia_principal*rendaindividual), by=domicilioid]
        tmp[ind_familia_principal == 0, rendadomiciliar:= NA]
        
        tmp[, rendadomiciliar_percap := rendadomiciliar/npessoas_fam_principal]
        
        tmp[, rendatodostrab_percap                     := sum(ind_familia_principal*rendatodostrab,na.rm=T)/npessoas_fam_principal, 
            by=domicilioid]
        tmp[, renda_todas_aposentadoria_e_pensao_percap := sum(ind_familia_principal*renda_todas_aposentadoria_e_pensao)/npessoas_fam_principal, 
            by=domicilioid]
        tmp[, renda_aluguel_percap                      := sum(ind_familia_principal*renda_aluguel)/npessoas_fam_principal, 
            by=domicilioid]
        tmp[, renda_todas_outras_percap                 := sum(ind_familia_principal*renda_todas_outras)/npessoas_fam_principal, 
            by=domicilioid]
        
        tmp[ind_familia_principal == 0, rendatodostrab_percap:= NA]
        tmp[ind_familia_principal == 0, renda_todas_aposentadoria_e_pensao_percap:= NA]
        tmp[ind_familia_principal == 0, renda_aluguel_percap:= NA]
        tmp[ind_familia_principal == 0, renda_todas_outras_percap:= NA]
        
        
        
        
        tmp = tmp[, list(  ano, domicilioid, rural, uf_padr, peso, 
                           
                           chefe_dom, conjuge_dom, ind_familia_principal, npessoas_fam_principal,
                           
                           sexo, idade, anosest, 
                           pea, cond_ocup, ind_empregador, ind_empregado, ind_conta_prop,
                           amostra_1976, amostra_1981, amostra_2004,
                           
                           ind_brancos_amarelos,
                           ind_pretos,
                           ind_pardos_indig_outr,
                           
                           horas_trabprinc, horas_todostrab,
                           ocup_original, setor_original,
                           carteira, estaturio_militar,
                           
                           rendatrab, 
                           rendatodostrab,
                           rendaindividual,
                           
                           rendadomiciliar,
                           rendadomiciliar_percap, 
                           
                           renda_todas_aposentadoria_e_pensao,
                           renda_aluguel,
                           renda_todas_outras,
                           
                           rendatodostrab_percap,
                           renda_todas_aposentadoria_e_pensao_percap,
                           renda_aluguel_percap,
                           renda_todas_outras_percap
                           
                           )]
        
        
        #tmp = tmp[ idade >= 10 ]
        
        cat("-------- liberando memoria\n")
        gc()
        
        cat("-------- fundindo o banco de", ano,"com os demais bancos\n")
        banco <- rbind(banco, tmp, use.names = T)
        cat("-------- liberando memoria\n")
        rm(list=setdiff(ls(),objetos_permanentes))
        Sys.sleep(.5)
        gc()
}
banco[ , pos_ocup := ind_empregado + 2*ind_conta_prop + 3*ind_empregador]

#rm(list=setdiff(ls(),"banco"))
gc()   



# Criando variaveis
banco[ , id_individuo := 1:nrow(banco)]
banco[ , lnrenda := log(rendatodostrab)]
banco[is.nan(lnrenda) | !is.finite(lnrenda), lnrenda := NA]
banco[ , regiao := trunc(uf_padr/10)]


