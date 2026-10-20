IM <- function(data, names = TRUE) {
  # Converte para variaveis Dummy para execucao da Analise
  # de Correspondencia Multipla, ou seja, em 0 e 1, caso dados nominais
  # Esta funcao e usada na funcao que balanceia dados Categoricos
  
  # Entrada:
  # data  - Dados Categoricos 
  # names - Incluir os nomes das variaveis nos niveis da Matriz Indicadora (default = TRUE).
  
  # Retorna:
  # mtxIndc - Dados convertidos em Matriz Indicadora
  
  if (!is.data.frame(data)) 
     data = as.data.frame(data)
  
  if (!is.logical(names)) 
     stop("Entrada para 'names' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  num.lin  <- nrow(data)  # Numero de linhas na tabela
  
  MFinal <- NULL
  for (k in 1:ncol(data)) {
    
     MConver   <- factor(data[,k]) # Matriz com os dados para a conversao
    
     Nivel     <- levels(MConver)  # Nomes dos Niveis
    
     Qtd_Nivel <- nlevels(MConver) # Quantidade de Niveis
    
     MDummy = matrix(0, num.lin, Qtd_Nivel) # Cria Matriz Vazia com elementos zero
    
     for (i in 1:Qtd_Nivel) {
      
       for (j in 1:num.lin) {
          if (MConver[j] == Nivel[i]) MDummy[j,i] <- 1
       }
      
     }
    
     if (names) {
        colnames(MDummy) <- paste(colnames(data)[k], Nivel, sep = ":") # Nomeia as colunas 
     } else {
        colnames(MDummy) <- Nivel # Nomeia as colunas  
     }
     
     MFinal <- cbind(MFinal, MDummy)
    
  }
  
  return(mtxIndc = MFinal)
}