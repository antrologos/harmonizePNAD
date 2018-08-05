padronizaPosOcup <- function(var_posOcup, 
                             tipo, 
                             ano,
                             var_posOcup2 = NULL  # variável que informa a posicao na 
                                                  # ocupacao na semana de referencia
                                                  # apenas para o ano de 1980
                             )
{
        
        if(tipo == "pnad" & (ano >= 1981 & ano <= 1990)){
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup==0]<- 0
                posOcup[var_posOcup==1]<- 1
                posOcup[var_posOcup==2]<- 1
                posOcup[var_posOcup==3]<- 1
                posOcup[var_posOcup==4]<- 1
                posOcup[var_posOcup==5]<- 3
                posOcup[var_posOcup==6]<- 3
                posOcup[var_posOcup==7]<- 2
                posOcup[var_posOcup==8]<- 2
                posOcup[var_posOcup==9]<- 0
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho principal na semana de referencia da pesquisa")
        }
        
        if(tipo == "pnad" & (ano >= 1992)){
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup==1] <- 1
                posOcup[var_posOcup==2] <- 1
                posOcup[var_posOcup==3] <- 1
                posOcup[var_posOcup==4] <- 1
                posOcup[var_posOcup==5] <- 1
                posOcup[var_posOcup==6] <- 1
                posOcup[var_posOcup==7] <- 1
                posOcup[var_posOcup==8] <- 1
                posOcup[var_posOcup==9] <- 3
                posOcup[var_posOcup==10] <- 2
                posOcup[var_posOcup==11] <- 0
                posOcup[var_posOcup==12] <- 0
                posOcup[var_posOcup==13] <- 0
                posOcup[var_posOcup==14] <- 0
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho principal na semana de referencia da pesquisa")
        
        }
        
        
        if(tipo == "censo" & ano == 1960){
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup==0] <- 0
                posOcup[var_posOcup==5 | var_posOcup==6] <- 1
                posOcup[var_posOcup==9] <- 2
                posOcup[var_posOcup==7 | var_posOcup==8] <- 3
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho 'habitual' do indivíduo")
        }
        
        if(tipo == "censo" & ano == 1970){
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup == 6] <- 0
                posOcup[var_posOcup == 1 | var_posOcup == 2] <- 1
                posOcup[var_posOcup == 5] <- 2
                posOcup[var_posOcup == 3 | var_posOcup == 4] <- 3
                
        }
        
        if(tipo == "censo" & ano == 1980){
                
                posOcup <- rep(NA, length(var_posOcup))
                
                if(!is.null(var_posOcup2)){
                        var_posOcup[!is.na(var_posOcup2)] <- var_posOcup2[!is.na(var_posOcup2)]
                }       
                
                posOcup[var_posOcup == 0 | var_posOcup == 9] <- 0
                posOcup[(var_posOcup >= 1 & var_posOcup <= 3) | var_posOcup == 6] <- 1
                posOcup[var_posOcup == 4 | var_posOcup == 7] <- 2
                posOcup[var_posOcup == 5 | var_posOcup == 8] <- 3
                        
                
        }
        
        if(tipo == "censo" & ano == 1991){
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup >= 1 & var_posOcup <= 8] <- 1
                posOcup[var_posOcup == 9] <- 3
                posOcup[var_posOcup == 10] <- 2
                posOcup[var_posOcup >= 11 & var_posOcup <= 14] <- 0
                
        }
        
        if(tipo == "censo" & ano == 2000){
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup >= 1 & var_posOcup <= 4] <- 1
                posOcup[var_posOcup == 5] <- 2
                posOcup[var_posOcup == 6] <- 3
                posOcup[var_posOcup >= 7 & var_posOcup <= 9] <- 0
        }
        
        if(tipo == "censo" & ano == 2010){
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup >= 1 & var_posOcup <= 4] <- 1
                posOcup[var_posOcup == 6] <- 2
                posOcup[var_posOcup == 5] <- 3
                posOcup[var_posOcup == 7] <- 0
        }
        #0 'Nao Remunerado'
        #1 'Empregado'
        #2 'Empregador' 
        #3 'Conta propria'.
        
        return(posOcup)
        
}