regression <- function(train, test = NA, class, intercept = TRUE) {
  # Funcao criada para realizar o metodo regressao linear em 
  # classificacao supervisionanda.
  # Devensolvida por Paulo Cesar Ossani em 29/07/2023.
  
  # Entrada:
  # train  - Matriz ou database dos dados de treinamento, sem as classes.
  # test   - Matriz ou database dos dados de teste (default = NA).
  # class  - Matriz com as classificacoes dos dados de train.
  # intercept - Considera o intercepto na regressao (default = TRUE).
  
  # Retorna:
  # predict - Os fatores classificados do conjunto test

  if (!is.data.frame(train) && !is.matrix(train))
     stop("Input 'train' is incorrect, must be data frame or matrix type. Check!")
  
  if (!is.na(class[1])) {
    
     class <- as.matrix(class)
    
     if (nrow(train) != length(class))
        stop("Input 'class' or 'train' is incorrect, must contain the same number of lines. Check!")
  }
  
  if (!is.logical(intercept)) 
     stop("'intercept' input is incorrect, it should be TRUE or FALSE. Verify!")
   
  if (is.factor(class) || is.character(class)) {
     names.class <- sort(unique(class))
     classes <- as.numeric(as.factor(class))
  }
 
  if (is.null(nrow(test)))
     test <- train
  
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")

  if (intercept) {
     model <- stats::lm(classes ~., data = train)
  } else model <- stats::lm(classes ~ 0 +., data = train)

  predict <- round(abs(predict(model, test, type = "response")))
 
  # transforma os resultados com os nomes das classes originais
  if (is.factor(class) || is.character(class)) {
     sort.pred <- sort(unique(predict))
     for(i in 1:length(sort.pred)) {
       name.pred <- ifelse(length(names.class[sort.pred[i]]) == 0 || is.na(names.class[sort.pred[i]]), "_unclassified", names.class[sort.pred[i]])
       predict <- replace(predict, predict == sort.pred[i], name.pred)
     }
  }

  message("\n \n End!")
  
  lista <- list(predict = as.factor(predict))

  return(lista)
  
}
