results <- function(orig.class, predict) {
  # Esta funcao desenvolvida para fornecer os
  # resultados na analise em machine leaning
  # por Paulo Cesar Ossani em 27/05/2021

  # Entrada:
  # orig.class - dados com as classes originais.
  # predict    - dados com as classes dos resultados dos classificadores.

  # Retorna:
  # mse  - Erro quadratico medio
  # mae  - Erro absoluto medio 
  # rae  - Erro absoluto relativo
  # conf.mtx   - matriz de confusao;
  # rate.hits  - taxa de acertos;
  # rate.error - taxa de erros;
  # num.hits   - numero de instancias corretas;
  # num.error  - numero de instancias erradas;
  # kappa      - estatistica kappa;
  # roc.curve  - dados para a curva ROC nas classes;
  # prc.curve  - dados para a curva PRC nas classes;
  # res.class  - resultados gerais para as classes: Sensitivity, Specificity,
  #              Precision, FP Rate, FN Rate, Negative Predictive Rate, F-Score, MCC, ROC Area, PRC Area

  if (length(orig.class) != length(predict))
     stop("Input 'orig.class' and 'predict' is incorrect, must contain the same number of observations. Check!")

  table.cont <- function(data, names.class) {
    # esta funcao cria a tabela de contigencia
    # foi necessaria devido a dejasuste com a funcao
    # interna do table() quando a classes que nao
    # foram classificadas, devendo estas estarem zeradas
    
    num.class <- length(names.class)
    table     <- matrix(0,ncol = num.class, nrow = num.class)
    colnames(table) <- names.class
    rownames(table) <- names.class
    for(i in 1:num.class) {
      for(j in 1:num.class) {
        table[i,j] = nrow(data[data[,1] == names.class[i] & data[,2] == names.class[j],])
      }
    }
    return(table)
  }
  ### FIM - Cria tabela de contingencia dos dados ###
  class <- as.factor(orig.class) # classes originais dos dados
  predc <- as.factor(predict)    # classes predita dos dados
  
  ## Incio - Erros na classficacao ##
  vlr.class <- as.numeric(class)
  vlr.prdct <- as.numeric(predict)
  num.obs   <- length(class) 
  mean.cls  <- mean(vlr.class)
  mse  <- sum((vlr.class - vlr.prdct)^2) / num.obs  # Erro quadratico medio
  mae  <- sum(abs(vlr.class - vlr.prdct)) / num.obs # Erro absoluto medio 
  rae  <- num.obs * mae / sum(abs(vlr.class - mean.cls)) # Erro absoluto relativo
  ## Incio - Erros na classficacao ##
  
  # class.names <- as.character(sort(unique(class)))    # nomes das classes
  # if(length(levels(class)) < length(levels(predc)))
  #    class.names <- as.character(sort(unique(predc))) # nomes das classes
  
  class.names <- as.character(sort(unique(c(levels(class),levels(predc)))))    # nomes das classes

  class.num <- length(class.names) # numero de classes
  
  conf.mtx <- table.cont(cbind(as.data.frame(class), predc), class.names) # matriz de confusao
  # conf.mtx <- table(class, predc) # matriz de confusao

  rate.hits  <- sum(diag(conf.mtx)) / sum(conf.mtx) # taxa de acertos
  rate.error <- 1 - rate.hits  # taxa de erros
  
  num.hits  <- sum(diag(conf.mtx)) # numero de sucessos
  num.inst  <- sum(conf.mtx)       # numero de instancias
  num.error <- num.inst - num.hits # numero de erros
  
  sum.col <- colSums(conf.mtx) # soma das colunas
  sum.lin <- rowSums(conf.mtx) # soma das linhas
  
  kappa <- 0
  if (length(diag(conf.mtx)) > 1) { # calcula o coeficiente kappa
     k2 <- sum(sum.col * sum.lin / num.inst^2) # porcentagem esperada de observacoes concordantes
     kappa <- (rate.hits - k2) / (1 - k2) # coeficiente kappa
  }
  
  diag <- diag(conf.mtx) # resultados positivos na predicao
  
  recall <- diag / sum.lin # sensitivity
  precis <- diag / sum.col # precision
  fmeasu <- 2 * recall * precis / (recall + precis) # F measure
  
  FPRate <- NULL # taxa de falsos positivos
  FNRate <- NULL # taxa de falsos negativos
  NPRate <- NULL # taxa de previsoes negativas
  specif <- NULL # especificidade
  MCC    <- NULL # coeficiente de correlacao de Matthews
  for(i in 1:class.num) {
  
    TP <- diag[i]
    TN <- num.inst - sum.col[i] - sum.lin[i] + TP
    FP <- sum.col[i] - TP
    FN <- sum.lin[i] - TP
    
    FPRate <- rbind(FPRate, (sum.col[i] - TP) / sum(sum.lin[-i]))
    
    FNRate <- rbind(FNRate, FN / (FN + TP))
      
    NPRate <- rbind(NPRate, TN / (TN + FN))
    
    specif <- rbind(specif, TN / (FP + TN))
    
    MCC <- rbind(MCC, (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN)))
  
  }
  
  ### Inicio - Dados para Curva ROC ###
  roc.curve <- as.data.frame(matrix(0, ncol = 3, nrow = 3 * class.num)) # dados para a curva ROC 
  prc.curve <- as.data.frame(matrix(0, ncol = 3, nrow = 3 * class.num)) # dados para a curva PRC 
  k <- 1
  for(i in 1:class.num) {
    roc.curve[k:(3*i),1] <- class.names[i]
    roc.curve[k,2:3] <- 1
    roc.curve[k+1,2] <- FPRate[i] # FP rate
    roc.curve[k+1,3] <- recall[i] # TP rate
    
    prc.curve[k:(3*i),1] <- class.names[i]
    prc.curve[k,2:3] <- c(1,0)
    prc.curve[k+1,2] <- recall[i] # TP rate
    prc.curve[k+1,3] <- precis[i] # FP rate
    prc.curve[k+2,2:3] <- c(0,1)
    
    k <- k + 3
  }
  ### Fim - Dados para Curva ROC ###
  
  ### Inicio - Calculo da area abaixo da curca ROC e PRC ###
  # Este calculo se baseia da soma das areas de triangulos
  # (poligono irregular) com base no uso de determinantes 
  AUC <- NULL
  PRC <- NULL
  for(i in 1:class.num) {
    AUC <- rbind(AUC, 0.5 * (recall[i] + 1 - FPRate[i]))
    PRC <- rbind(PRC, 0.5 * (precis[i] + recall[i]))
  }
  ### Fim - Calculo da area abaixo da curca ROC e PRC ###
   
  res.class <- as.data.frame(matrix(NA, nrow = (nrow(conf.mtx) + 1), ncol = 11))
  colnames(res.class)       <- c("Class","Sensitivity","Specificity","Precision","FP Rate","FN Rate", 
                                 "Negative Predictive Rate","F-Score","MCC","ROC Area","PRC Area")
  res.class[,"Class"]       <- c(rownames(conf.mtx), "Weighted average")
  res.class[,"Sensitivity"] <- c(round(recall,4), round(sum(recall * sum.lin / num.inst),4))
  res.class[,"Specificity"] <- c(round(specif,4), round(sum(specif * sum.lin / num.inst),4))
  res.class[,"Precision"]   <- c(round(precis,4), round(sum(precis * sum.lin / num.inst),4))
  # res.class[,"TP Rate"]     <- c(round(recall,4), round(sum(recall * sum.lin / num.inst),4))
  res.class[,"FP Rate"]     <- c(round(FPRate,4), round(sum(FPRate * sum.lin / num.inst),4))
  res.class[,"FN Rate"]     <- c(round(FNRate,4), round(sum(FNRate * sum.lin / num.inst),4))
  res.class[,"Negative Predictive Rate"]     <- c(round(NPRate,4), round(sum(NPRate * sum.lin / num.inst),4))
  res.class[,"F-Score"]     <- c(round(fmeasu,4), round(sum(fmeasu * sum.lin / num.inst),4))
  res.class[,"MCC"]         <- c(round(MCC,4), round(sum(MCC * sum.lin / num.inst),4))
  res.class[,"ROC Area"]    <- c(round(AUC,4), round(sum(AUC * sum.lin / num.inst),4))
  res.class[,"PRC Area"]    <- c(round(PRC,4), round(sum(PRC * sum.lin / num.inst),4))
  
  list <- list(mse = mse, mae = mae, rae = rae,
               conf.mtx = conf.mtx, rate.hits = rate.hits,
               rate.error = rate.error, num.hits = num.hits,
               num.error = num.error, kappa = kappa,
               roc.curve = roc.curve, prc.curve = prc.curve,
               res.class = res.class)
  
  return(list)
}
