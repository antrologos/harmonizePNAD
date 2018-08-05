cria_domicilioid73 = function(x){   #  x: variável de posição no domicílio ("v0154 - Relação com chefe").
                                    # essa função supõem que o banco de dados esteja originalmente ordenado
                                    # por domicílios e que as pessoas dentro dos domicílios também estejam ordenadas 
                                    # Ou seja: assume-se que o banco de dados inicia-se com um caso de chefe de
                                    # domicílio, passando para outros membros (esposa/compalheira, filhos/enteados, etc)
                                    # então segue-se um novo domicílio, também iniciado pelo chefe... e assim por diante
        
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
