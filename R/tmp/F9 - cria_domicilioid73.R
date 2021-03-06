cria_domicilioid73 = function(x){   #  x: vari�vel de posi��o no domic�lio ("v0154 - Rela��o com chefe").
                                    # essa fun��o sup�em que o banco de dados esteja originalmente ordenado
                                    # por domic�lios e que as pessoas dentro dos domic�lios tamb�m estejam ordenadas 
                                    # Ou seja: assume-se que o banco de dados inicia-se com um caso de chefe de
                                    # domic�lio, passando para outros membros (esposa/compalheira, filhos/enteados, etc)
                                    # ent�o segue-se um novo domic�lio, tamb�m iniciado pelo chefe... e assim por diante
        
        chefe = rep(0, length(x))
        chefe[x %in% c(1,2)] = 1
        
        cont = 1:length(x)
        
        chef_cont = chefe*cont
        
        for(i in 1:length(chef_cont)){
                
                if( chef_cont[i] != 0){
                        dom = chef_cont[i]
                }else{
                        chef_cont[i] = dom
                }
        }
        return(chef_cont)
}
