brute.force <- function(func = NA, train, test, class.train,
                        class.test, args = NA, measure = "Rate Hits", 
                        output = 10) {
  # Funcao criada para executar o metodo da forca bruta para a selecao 
  # de variaveis, desenvolvida por Paulo Cesar Ossani em 22/06/2024.
  
  # Entrada:
  # func  - Fucao de classsificao supervisionda a ser analisada.
  # train - Matriz ou database dos dados de treinamento, sem as classes.
  # test  - Matriz ou database dos dados de teste.
  # class.train - Vetor com nomes de classes de dados de treinamento.
  # class.test - Vetor com nomes de classes de dados de teste.
  # args  - Argumento usando no classificador dando em func.
  # measure - Medida para avaliar o modelo: 
  #           "Rate Hits" (default), "Kappa Index", "Sensitivity", 
  #           "Specificity", "Precision", "FP Rate", "FN Rate", 
  #           "Negative Predictive Rate", "F-Score", "MCC", 
  #           "ROC Are" or "PRC Area"
  #           Se measure = NA retorna todas as metricas ordenadas por 'Rate Hits'
  # output - Numero de elementos com as melhores combinacoes de variaveis na matriz 'best.model' (defaul = 10)
  
  # Retorna:
  # best.model - Matriz com os nomes das melhores combinacoes de variaveis, conforme medida de avaliacao usada: acuracia, precisao, recall etc.
  # text.model - Estrutura do modelo de classificacao usado.
  
  
  if (!(func %in% c("knn","lda","qda","regression")))
     stop("Input 'func' is incorrect, must be: 'knn', 'lda', 'qda' or 'regression'. Check!")
  
  
  if (!is.data.frame(train) && !is.matrix(train))
     stop("Input 'train' is incorrect, must be data frame or matrix type. Check!")

  # if (!is.data.frame(train) && !is.matrix(train))
  #    stop("Input 'train' is incorrect, must be data frame or matrix type. Check!")

  if (!is.na(class.train[1])) {

     class.train <- as.matrix(class.train)

     if (nrow(train) != length(class.train))
        stop("Input 'class.train' or 'train' is incorrect, must contain the same number of lines. Check!")
  }

  if (!is.na(measure)) {
     measure <- tolower(measure) # torna minusculo
     if        (measure == "hits"  || measure == "rate hits")  { measure = "Rate Hits" 
     } else if (measure == "kappa" || measure == "kappa index"){ measure = "Kappa Index"
     } else if (measure == "sens"  || measure == "sensitivity"){ measure = "Sensitivity"
     } else if (measure == "spec"  || measure == "specificity"){ measure = "Specificity"
     } else if (measure == "prec"  || measure == "precision")  { measure = "Precision"
     } else if (measure == "fp"    || measure == "fp rate")    { measure = "FP Rate"
     } else if (measure == "fn"    || measure == "fn rate" )   { measure = "FN Rate"
     } else if (measure == "npr"   || measure == "negative predictive rate"){ measure = "Negative Predictive Rate"
     } else if (measure == "fscore" || measure == "F-Score")   { measure = "F-Score"
     } else if (measure == "mcc")                              { measure = "MCC"
     } else if (measure == "roc"     || measure == "roc are")  { measure = "ROC Are"
     } else if (measure == "prc"     || measure == "prc area") { measure = "PRC Area" }
  }
  var.measure <- c('Rate Hits','Kappa Index','Sensitivity','Specificity',
                   'Precision','FP Rate','FN Rate','Negative Predictive Rate',
                   'F-Score','MCC','ROC Are','PRC Area',NA)
  if (!(measure %in% var.measure))
     stop(paste("Input 'measure' is incorrect, it should be: ",
                paste(var.measure, sep = ",", collapse = ","),". Verify!", sep = ""))
  
  if (output < 2) 
     stop("Input 'output' is incorrect, must be positive integers numbers greater than one. Verify!")
  
  all.vars <- colnames(train)  # nomes das variaveis
  num.var  <- length(all.vars) # numero de variaveis
  
  num.comb <- 2^num.var - (num.var + 1) # numero de combinacoes
  
  message("\014") # limpa a tela

  func.best.model <- function() { # retorna as 'output' melhores posicoes
    
    pos.mtx <- match(measure, var.measure)
  
    mo <- unlist(best.model[,pos.mtx])
    names(mo) <- 1:length(mo)
    srt  <- as.numeric(names(sort(mo, decreasing = TRUE))) # ordena as respostas em ordem descendente

    best.model <- best.model[srt[1:output],]
    
    return(best.model)
  }

  ## Inicio - cria a estrutura no modelo de classificacao ##
  if (func == "lda" || func == "qda") {
     text.model <- paste(func,"(data = train[,comb[,j]], class = class.train, ",sep="")#,test = test[,comb[,j]],class = class,", sep = "")
  } else {
     text.model <- paste(func,"(train = train[,comb[,j]], class = class.train, ",sep="")#,test = test[,comb[,j]],class = class,", sep = "")
  }
  
  if (func == "lda" || func == "qda") {
     text.model <- paste(text.model,"test = test, ", sep = "")
  } else if (is.data.frame(test)) {
     text.model <- paste(text.model,"test = test[,comb[,j]], ", sep = "")
  }
  
  if (!is.na(args))
    text.model <- paste(text.model,args,sep = "")
  text.model <- paste(text.model,")", sep = "")
  ## Fim - cria a estrutura no modelo de classificacao ##
 
  best.model <- NULL # matriz com 'output' linhas em ordem descendentes com as melhores combinacoes de variaveis
  
  npro <- 0 # contador de processos
  for (i in num.var:2) {
 
    comb <- combn(all.vars, i) # combinacao das variaveis
    
    for (j in 1:ncol(comb)) {
 
      start.time <- as.numeric(Sys.time()) # tempo inicial do processo
      
      model <- suppressMessages(eval(parse(text = text.model)))

      res.class <- results(orig.class = class.test, predict = model$predict)
 
      sev.res    <- res.class$res.class
      best.model <- rbind(best.model, c(as.data.frame(res.class$rate.hits), res.class$kappa,
                                        sev.res[nrow(sev.res), 2:ncol(sev.res)],
                                        paste(comb[,j], collapse = " ")))
      
      rm(model, sev.res, res.class) # limpa memoria
      
      npro <- npro + 1
      end.time  <- as.numeric(Sys.time())
      time.left <- (num.comb - npro) * (end.time - start.time)
      print(paste("Process: ", npro, "/", num.comb, 
                  " Estimated processing time: days ", round(time.left / 86400,2), 
                  " hours: ", round(time.left / 3600, 2),
                  " minutes: ", round(time.left / 60, 2),sep=""))

    }
    
    if (nrow(best.model) > output) best.model <- func.best.model() # retorna as 'output' melhores posicoes

  }
  
  if (output > nrow(best.model)) output <- nrow(best.model)
  
  best.model <- func.best.model()
  
  colnames(best.model) <- c(var.measure[1:12],"Variable Names")
  
  if (!is.na(measure)) {
     pos.mtx    <- match(measure, var.measure)
     best.model <- best.model[,c(pos.mtx, ncol(best.model))]
  }

  lista <- list(best.model = best.model, text.model = text.model)

  return(lista)
}
