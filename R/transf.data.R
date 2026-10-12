transf.data <- function(dados) {
  # Funcao que transforma dados categoricos em binarios
  for (col in names(dados)) {
    
    if (is.factor(dados[[col]]) || is.character(dados[[col]])) { # Verifica se a coluna é categórica (fator ou caractere)
      # message(sprintf("Convertendo a variável categórica: '%s'", col))
      
      resul.bin <- IM(dados[[col]]) # transforma dados categoricos em binaria
      
      dados[[col]] <- NULL # Remove a coluna categórica original do dataset
      
      dados <- cbind(dados, as.data.frame(resul.bin))
    }
    
  }
  
  # quando houver nomes iguais acrescenta numeros para tornarem unicos
  names(dados) <- make.unique(names(dados), sep = "")
  
  return(dados)
}
