kmeans <- function(data, normalize = FALSE, num.groups = 2) {
  
  # Esta funcao executa a analises de agrupamentos nao-hierarquicos(k-means), 
  # desenvolvida por Paulo Cesar Ossani em 29/07/2023
  
  # Entrada:
  # data - Dados a serem a analizados
  # normalize  - Normalizar os dados (default = FALSE).
  # num.groups - Numero de grupos a formar (default = 2).

  # Retorna:
  # groups - Dados originais com os grupos formados.
  # res.groups - Resultados dos grupos formados.
  # sum.sqt - Soma do quadrado total.
  # R.sqt   - R quadrado

  if (!is.data.frame(data)) 
     stop("'data' input is incorrect, it should be of type data frame. Verify!!")

  if (!is.logical(normalize)) 
     stop("'normalize' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (is.na(num.groups)) num.groups <- 0 # numero de grupos a formar
  
  if (num.groups >= nrow(data) )
     stop("'num.groups' input is high. Verify!")
  
  if (num.groups < 0)
     stop("'num.groups' input is incorrect, must be positive integers greater than 1. Verify!")

  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")
  
  data.new <- data # dados a serem analizados
  
  if (normalize)
     data.new <- scale(data.new) # normaliza por colunas os dados

  #set.seed(7) # semente para fixar processo heuristico
      
  hc <- stats::kmeans(data.new, num.groups, iter.max = 100) # executa o method K-Means
  #,iter.max = 100, nstart = 21, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")) # cria particoes pelo method K-means
      
  #fitted(hc, method = c("centers", "classes"))
  groups <- hc$cluster
     
  m.groups  <- cbind(data, groups) # matriz com dados originais mais os grupos formados
     
  ### INICIO - analises dos grupos ###
  sum.sqt <- NA # soma do quadrado total 
  tab.res.groups <- NULL # tabela com os resultados dos grupos
  mtx.groups <- cbind(data.new, groups) # matriz com dados originais mais os grupos formados
  mean.g <- apply(data.new, 2, mean)
  SSB   <- 0 # soma de quadrado entre grupos
  R.sqt <- 0 # R quadrado
  for (i in 1:num.groups) { 
      new.groups   <- subset(mtx.groups, groups == i) 
      groups.calc  <- new.groups[,1:(ncol(new.groups)-1)]
      qtd.elements <- nrow(new.groups)
        
      if (qtd.elements == 1) mean <- groups.calc else
         mean <- apply(groups.calc, 2, mean)
        
      if (qtd.elements == 1) SqG <- 0 else # soma dos quadrados dos grupos
         SqG <- sum(sweep(groups.calc, 2, mean)^2) # soma dos quadrados dos grupos
      
      SSB <- SSB + sum(qtd.elements * (apply(groups.calc, 2, mean) - mean.g)^2) # soma de quadrado entre grupos
      
      tab.res.groups <- rbind(tab.res.groups,c(i, qtd.elements, SqG, mean))
  }
  colnames(tab.res.groups) <- c("groups", "Number of Elements", "Sum of Squares",paste("Mean", colnames(tab.res.groups[,4:(ncol(tab.res.groups))])))
    
  sum.sqt <- sum(sweep(data.new, 2, apply(data.new, 2, mean))^2) # soma do quadrado total
 
  R.sqt <- SSB / sum.sqt # R quadrado
  ### FIM - analises dos grupos ###
  
  message("\n \n End!")
  
  lista <- list(groups = m.groups, res.groups = tab.res.groups, sum.sqt = sum.sqt, R.sqt = R.sqt)
  
  return(lista)
}