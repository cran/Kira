qda <- function(data, test = NA, class = NA, type = "train", 
                method = "moment", prior = NA) {

  # Esta funcao executa a Analide resultriminante quadratica
  # desenvolvida por Paulo Cesar Ossani em 24/07/2023
  
  # Entrada:
  # data  - Dados a serem a classificados.
  # test  - Vetor com os indices que serao utilizados em 'data' como teste (default = NA). Para type = "train", tem-se test = NA.
  # class - Vetor com os nomes das classes dos dados.
  # type  - Tipo de validacao: "train" - Treinamento dos dados (default), ou "test" - classifica os dados do vetor "test".
  # method - Metodo de classificacao: "mle" para MLEs, "mve" para usar cov.mv, "moment" (default) para estimadores padrao da media e variancia ou "t" para estimativas robustas baseadas em uma distribuicao t.
  # prior  - Probabilidades de ocorrencia das classes. Se nao especificado, tomara as proporcoes das classes. Se especificado, as probabilidades devem seguir a ordem dos niveis dos fatores.
   
  # Retorna:
  # predict - Os fatores classificados do conjunto test

  if (!is.data.frame(data)) 
     stop("'data' input is incorrect, it should be of type data frame or matrix. Verify!")
  
  if (any(NA %in% class)) {

     class <- as.matrix(class)

     if (nrow(data) != length(class))
        stop("'class' or 'data' input is incorrect, they should contain the same number of lines. Verify!")
  }
  
  if (!is.na(prior[1]) && sum(prior) != 1)
     stop("The sum of the elements in 'prior' must be equal to one. Verify!")
  
  type <- tolower(type) # torna minusculo
  if (!(type %in% c("train","test")))
     stop("'type' input is incorrect, it should be: 'train' or 'test'. Verify!")
  
  if (type == "test" && is.na(test[1]))
     stop("Input for type = 'test', the 'test' vector must be added. Verify!")
  
  method <- tolower(method) # torna minusculo
  if (method == "mom") method = "moment"
  if (!(method %in% c("mle","mve","moment","t")))
     stop("'method' input is incorrect, it should be: 'mle', 'mve', 'moment' or 't'. Verify!")
  
  if (type == "train" && !is.na(test[1]))
     stop("For type = 'train', 'test' should be equal to 'NA'. Verify!")
  
  if (length(class) < (nrow(data)-length(test)))
     stop("The number of elements in 'class' must be equal to the number of elements in 'data'. Verify!")
  
  if (!is.na(class[1])) {
     class.table <- table(class)        # cria tabela com as quantidade dos elementos das classes
     class.names <- names(class.table)  # nomes das classses
     num.class   <- length(class.table) # numero de classes
  } else {
     num.class <- 1
  }

  if (!is.na(prior[1]) && length(prior) != num.class)
     stop("The number of elements in 'prior' must be equal to the number of classes. Verify!")
  
  if (is.na(prior[1])) # caso probabilidade a priori nao seja informada
     prior <- as.double(rep(1,num.class)/num.class)
  
  if (type == "train")
     train = as.integer(rownames(data))
  
  if (type == "test" && !is.na(test[1]))
     train = as.integer(rownames(data[-test,]))
  
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")
     
  result <- MASS::qda(class~., data, prior = prior, method = method, subset = train)

  if (type == "train" || is.na(test[1])) train = -train
  
  predict <- predict(result, data[-train,])$class
  
  message("\n \n End!")
  
  lista <- list(predict = as.factor(predict))
  
  return(lista)
}
