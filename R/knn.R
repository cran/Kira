knn <- function(train, test, class, k = 1, dist = "euclidean", lambda = 3) {
  # Funcao criada para realizar o metodo kNN.
  # Devensolvida por Paulo Cesar Ossani em 24/09/2021.
  
  # Entrada:
  # train  - Matriz ou database dos dados de treinamento, sem as classes.
  # test   - Matriz ou database dos dados de teste.
  # class  - Matriz com as classificacoes dos dados de train.
  # k      - Numeros de visinhos a considerar (default = 1).
  # dist   - Distancia usada no processo (default = EUCLIDEAN). 
  #          Pode-se usar as distancias: "euclidean", "manhattan", "minkowski", "canberra", "maximum" or "chebyshev".
  # lambda - Numero usado na distancias de Minkowski (default = 3).
  
  # Retorna:
  # predict - Os fatores classificados do conjunto test

  if (!is.data.frame(train) && !is.matrix(train))
     stop("Input 'train' is incorrect, must be data frame or matrix type. Check!")
  
  if (!is.na(class[1])) {
    
     class <- as.matrix(class)
    
     if (nrow(train) != length(class))
        stop("Input 'class' or 'train' is incorrect, must contain the same number of lines. Check!")
  }
  
  dist <- toupper(dist) # transforma em maiusculo
  if      (dist == "EUC") dist = "EUCLIDEAN"
  else if (dist == "MAN") dist = "MANHATTAN"  
  else if (dist == "MIN") dist = "MINKOWSKI" 
  else if (dist == "CAN") dist = "CANBERRA" 
  else if (dist == "MAX") dist = "MAXIMUM" 
  else if (dist == "CHE") dist = "CHEBYSHEV"
  if (!(dist %in% c("EUCLIDEAN", "MANHATTAN", "MINKOWSKI", "CANBERRA", "MAXIMUM", "CHEBYSHEV")))
     stop("Input 'dist' is incorrect, it should be: 'euclidean', 'manhattan', 'minkowski', 'canberra', 'maximum' or 'chebyshev'. Check!")
  
  if (k < 0 && !(k == round(k)))
     stop("Input 'k' is incorrect, must be positive integers numbers. Check!")
  
  if (lambda < 0) # && !(lambda == round(lambda)))
     stop("Input 'lambda' is incorrect, must be a number greater than zero. Check!")
  
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")
  
  if (dist == "EUCLIDEAN") ndist = 1 # euclidean distance
  if (dist == "MANHATTAN") ndist = 2 # manhattan distance
  if (dist == "MINKOWSKI") ndist = 3 # ninkowski distance
  if (dist == "CANBERRA")  ndist = 4  # canberra distance
  if (dist == "MAXIMUM" || dist == "CHEBYSHEV") ndist = 5 # maximum (Chebyshev) distance
  
  nlin_train <- nrow(train)
  nlin_test  <- nrow(test)
  
  res <- .C("knn", row_train = as.integer(nlin_train), row_test = as.integer(nlin_test),
            col = as.integer(ncol(train)), train = as.double(as.matrix(train)), test = as.double(as.matrix(test)),
            mtd = as.integer(ndist), k = as.integer(k), lambda = as.double(lambda),
            neigh = as.matrix(rep(0.0, k * nlin_test)))
  
  predict <- NULL # matriz de predicao
  resul   <- res$neigh
  j = 1
  for(i in seq(1,length(res$neigh), by = k)) {
    
    tb <- table(class[c(res$neigh[i:(i+k-1)])]) # tabela com as frequencias de ocorrencia das classes
    
    predict[j] <- names(tb[which(tb == max(tb))][1]) # resultado com maior ocorrencia
    
    j = j + 1
    
  }

  message("\n \n End!")
  
  lista <- list(predict = as.factor(predict))

  return(lista)
  
}
