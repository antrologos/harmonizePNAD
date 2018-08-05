padronizaUF <- function(var_UF,
                        ano){
  
        
  ano = as.numeric(ano)
  nova_UF = rep(as.numeric(NA), length(var_UF))
   
  if(ano == 1973){
          # recodifica os codigos de 1973 conforme o padrao
          # adotado nos anos 1990, 2000 e 2010
          nova_UF[var_UF ==71] <- 11
          nova_UF[var_UF ==72] <- 12
          nova_UF[var_UF ==73] <- 13
          nova_UF[var_UF ==74] <- 14
          nova_UF[var_UF ==75] <- 15
          nova_UF[var_UF ==76] <- 16
          nova_UF[var_UF ==51] <- 21
          nova_UF[var_UF ==52] <- 22
          nova_UF[var_UF ==53] <- 23
          nova_UF[var_UF ==54] <- 24
          nova_UF[var_UF ==55] <- 25
          nova_UF[var_UF ==56] <- 26
          nova_UF[var_UF ==57] <- 27
          nova_UF[var_UF ==58] <- 28
          nova_UF[var_UF ==59] <- 29
          nova_UF[var_UF ==41] <- 31
          nova_UF[var_UF ==42] <- 32
          nova_UF[var_UF ==11] <- 33
          nova_UF[var_UF ==12] <- 33
          nova_UF[var_UF ==21] <- 35
          nova_UF[var_UF ==31] <- 41
          nova_UF[var_UF ==32] <- 42
          nova_UF[var_UF ==33] <- 43
          nova_UF[var_UF ==81] <- 51 # Inclui Mato Grosso e Mato Grosso do Sul
          nova_UF[var_UF ==82] <- 52 # Inclui Tocantis e Goias
          nova_UF[var_UF ==61] <- 53
  }
  
  if(ano >= 1976 & ano <= 1979){
                # recodifica os codigos dos anos de 1976 a 1979 conforme o padrao
                # adotado nos anos 1990, 2000 e 2010
                  nova_UF[var_UF ==71] <- 11
                  nova_UF[var_UF ==72] <- 12
                  nova_UF[var_UF ==73] <- 13
                  nova_UF[var_UF ==74] <- 14
                  nova_UF[var_UF ==75] <- 15
                  nova_UF[var_UF ==76] <- 16
                  nova_UF[var_UF ==51] <- 21
                  nova_UF[var_UF ==52] <- 22
                  nova_UF[var_UF ==53] <- 23
                  nova_UF[var_UF ==54] <- 24
                  nova_UF[var_UF ==55] <- 25
                  nova_UF[var_UF ==56] <- 26
                  nova_UF[var_UF ==57] <- 27
                  nova_UF[var_UF ==58] <- 28
                  nova_UF[var_UF ==59] <- 29
                  nova_UF[var_UF ==41] <- 31
                  nova_UF[var_UF ==43] <- 32
                  nova_UF[var_UF ==11] <- 33
                  nova_UF[var_UF ==21] <- 35
                  nova_UF[var_UF ==31] <- 41
                  nova_UF[var_UF ==32] <- 42
                  nova_UF[var_UF ==33] <- 43
                  nova_UF[var_UF ==77] <- 51
                  nova_UF[var_UF ==78] <- 52
                  nova_UF[var_UF ==61] <- 53
  }
  
  
  if(ano >= 1981 & ano <= 1990){
                  # recodifica os codigos dos anos 1980 conforme o padrao
                  # adotado nos anos 1990, 2000 e 2010
                  nova_UF[var_UF %in% 11:14] <- 33
                  
                  nova_UF[var_UF %in% 20:29] <- 35
                  
                  nova_UF[var_UF %in% c(30:31,37)] <- 41
                  nova_UF[var_UF == 32] <- 42
                  
                  nova_UF[var_UF %in% 33:35] <- 43
                  
                  
                  nova_UF[var_UF %in% c(41:42,44)] <- 31
                  
                  nova_UF[var_UF == 43] <- 32
                  nova_UF[var_UF == 51] <- 21
                  nova_UF[var_UF == 52] <- 22
                  nova_UF[var_UF == 53] <- 23
                  nova_UF[var_UF == 54] <- 24
                  nova_UF[var_UF == 55] <- 25
                  nova_UF[var_UF == 56] <- 26
                  nova_UF[var_UF == 57] <- 27
                  nova_UF[var_UF == 58] <- 28
                  nova_UF[var_UF %in% 59:60] <- 29
                  
                  nova_UF[var_UF == 61] <- 53
                  nova_UF[var_UF == 71] <- 11
                  nova_UF[var_UF == 72] <- 12
                  nova_UF[var_UF == 73] <- 13
                  nova_UF[var_UF == 74] <- 14
                  nova_UF[var_UF == 75] <- 15
                  nova_UF[var_UF == 76] <- 16
                  nova_UF[var_UF == 81] <- 51 # Mato Grosso do Sul -> Mato Grosso 
                  nova_UF[var_UF == 82] <- 51 # Mato Grosso        -> Mato Grosso
                  nova_UF[var_UF == 83] <- 52
  }
  
  
  if(ano >= 1992){
    nova_UF = var_UF
    nova_UF[var_UF == 17] <- 52 # funde Tocantins e Goias
    nova_UF[var_UF == 50] <- 51 # funde Mato Grosso do Sul e Mato Grosso 
    
  }
  
 
  
  return(nova_UF)
}

