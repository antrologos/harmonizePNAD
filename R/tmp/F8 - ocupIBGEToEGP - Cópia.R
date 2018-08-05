ocupIBGEToEGP <- function(ocup,  # nome da variavel que contem a classificacao das ocupacoes
                          tipo,     # character: "censo", "pnad" ou "pnadc"
                          ano= NULL){ # numeric: nao eh preciso especificar ano para a pnadc {
        
        banco <- data.table(var_ocup = double(length(ocup)),
                            egp = double(length(ocup)))
        
        banco[, var_ocup := ocup]
        
        
        if((tipo == "censo" & ano == 2010)| (tipo == "pnadc")) {

        }
        
        
        if((tipo == "censo" & ano == 2000) | (tipo == "pnad" & ano >= 2002 & ano <= 2014)){
               
                
        }
        
        
        if((tipo == "censo" & ano == 1991) | (tipo == "pnad" & (ano >= 1992 & ano <= 2001))){
               
                
        }
        
        
        if((tipo == "censo" & ano == 1980) | (tipo == "pnad" & (ano >= 1981 & ano <= 1990))){
              
                
        }
        
        
        if(tipo == "pnad" & ano >= 1976 & ano <= 1979 ) {
               
                
        }
        
        
        if( (tipo == "censo" & ano == 1970) | (tipo == "pnad" & ano == 1973 ) ) {
               
                
        }
        
        
        if(tipo == "censo" & ano == 1960){
                
                
        }
        
        
        banco[egp == 0, egp := NA]
        banco[egp %in% c(5,6), egp := 1]
        
        return(banco[,egp])
}