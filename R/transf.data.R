transf.data <- function(data) {
  # Converte variaveis categorias em Dummy quando houver em 'data'
  # desenvolvida em 11/10/2026
  
  # Entrada:
  # data  - Dados Numericos e Categoricos 

  # Retorna:
  # trf.data - Dados numercicos com dados categoricos convertidos em binarios
  
  if (!is.data.frame(data)) 
     data = as.data.frame(data)
  
  num.var <- any(!sapply(sapply(data, class), function(x) any(x %in% c("numeric", "integer"))))
  if (num.var) { # verifica se ha variaveis categoricas
    
    # dados numericos
    dat.num <- data[,sapply(sapply(data, class), function(x) any(x %in% c("numeric", "integer")))]
                               
    # dados categoricos
    dat.cat <- data[,!sapply(sapply(data, class), function(x) any(x %in% c("numeric", "integer")))]
    
    resul.bin <- IM(data = dat.cat, names = TRUE) # transforma dados categoricos em binarios
    
    data <- cbind(dat.num, resul.bin)
    
    # quando houver nomes iguais acrescenta numeros para tornarem unicos
    names(data) <- make.unique(names(data), sep = "")
  }
  
  return(data)
}
