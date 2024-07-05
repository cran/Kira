vote <- function(mtx.algtms = NA) {
  # Funcao criada para executar o metodo vote para a classificacao
  # supervisionada, desenvolvida por Paulo Cesar Ossani em 24/06/2024.
  
  # Entrada:
  # mtx.algtms - matriz com os resultados dos algoritmos de classificacao supervisionada a serem analisados.

  # Retorna:
  # predict - os fatores classificados do conjunto teste
  
  rownames(mtx.algtms) <- NULL
  
  if (is.null(ncol(mtx.algtms)))
     stop("To execute the vote algorithm, more than one classifier is needed. Verify!")
  
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")
  
  choose.best.model <- function(x) { # funcao que escolhe o melhor modelo
    sorted <- sort(table(x), decreasing = TRUE)
    best   <- names(sorted)[1]
    return(best)
  }
  
  predict <- apply(mtx.algtms, 1, choose.best.model)
  
  message("\n \n End!")
  
  lista <- list(predict = as.factor(predict))
  
  return(lista)
} 
