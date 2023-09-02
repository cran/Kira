hierarchical <- function(data, titles = NA, analysis = "Obs", cor.abs = FALSE,  
                    normalize = FALSE, distance = "euclidean", method = "complete", 
                    horizontal = FALSE, num.groups = 0, lambda = 2, savptc = FALSE,
                    width = 3236, height = 2000, res = 300, casc = TRUE) {
                    
  # Esta funcao executa a analises de agrupamentos hierarquicos,
  # desenvolvida por Paulo Cesar Ossani em 29/07/2023
  
  # Entrada:
  # data - Dados a serem a analizados
  # titles - Titulos para os graficos.
  # analysis - "Obs" para analysiss nas observacoes (default),
  #           "Var" para analysiss nas variaveis.
  # cor.abs  - Matriz de correlacao absoluta caso analysis = "Var" (default = FALSE).
  # normalize - normalizar os dados somente para caso analysis = "Obs" (default = TRUE).
  # distance  - Metrica das distancias caso agrupamentos hierarquicos:
  #             "euclidean" (default), "maximum", "manhattan",
  #             "canberra", "binary" ou "minkowski". Caso analysis = "Var" a metrica
  #              sera a matriz de correlacao, conforme cor.abs.
  # method - Metodo para analysiss caso agrupamentos hierarquicos:
  #          "complete" (default), "ward.D", "ward.D2", "single",
  #          "average", "mcquitty", "median" ou "centroid".
  # horizontal - Dendrograma na horizontal (default = FALSE).
  # num.groups - Numero de grupos a formar.
  # lambda - Valor usado na distancia de minkowski.
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos.
  # tab.res - Tabela com as similaridades e distancias dos grupos formados.
  # groups - Dados originais com os grupos formados.
  # res.groups - Resultados dos grupos formados.
  # sum.sqt - Soma do quadrado total.
  # R.sqt   - R quadrado
  # mtx.dist - Matriz das distancias.
  
  if (!is.data.frame(data)) 
     stop("'data' input is incorrect, it should be of type data frame. Verify!!")

  analysis <- toupper(analysis)
  
  if (analysis != "OBS" && analysis != "VAR") 
     stop("'analysis' input is incorrect, it should be 'Obs' for the observations or 'Var' for the variables. Verify!")

  if (!is.logical(cor.abs)) 
     stop("'cor.abs' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (!is.logical(normalize)) 
     stop("'normalize' input is incorrect, it should be TRUE or FALSE. Verify!")

  distance <- tolower(distance) # torna minusculo

  if (!(distance %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")))
     stop("'distance' input is incorrect, it should be: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verify!")
  
  meth <- c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median" , "centroid")
  if (!(method %in% meth)) 
     stop("'method' input is incorrect, it should be: 'complete', 'ward.D', 
          'ward.D2', 'single', 'average', 'mcquitty', 'median' ou 'centroid'. Verify!")
  
  if (!is.logical(horizontal)) 
     stop("'horizontal' input is incorrect, it should be TRUE or FALSE. Verify!")

  if (is.na(num.groups)) num.groups <- 0 # numero de grupos a formar
  
  if (num.groups >= nrow(data) )
     stop("'num.groups' input is high. Verify!")
  
  if (num.groups < 0)
     stop("'num.groups' input is incorrect, it should be positive integer numbers, or zero. Verify!")

  if (!is.numeric(lambda) || lambda <= 0)
     stop("'lambda' input is incorrect, it is necessary lambda > 0. Verify!") 
           
  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(casc && !savptc))
     stop("'casc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end!")
  
  data.new <- data # dados a serem analizados
  
  if (normalize && analysis == "OBS")
     data.new <- scale(data.new) # normaliza por colunas os dados

  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Graph of the similarity\n within groups")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Graph of the distances\n within of the groups")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Dendrogram")
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Saving graphics to hard disk. Wait for the end!")
  }
    
  if (analysis == "OBS") # analysis nas observacoes
     mtx.dist <- dist(data.new, method = distance, p = lambda) # matrix das distancias
   
  if (analysis == "VAR") {# analysis nas variaveis
     if (cor.abs) # matrix de correlacao absoluta
         mtx.dist <- as.dist(1 - abs(cor(data))) # matrix das distancias
        
     if (!cor.abs) # matrix de correlacao
         mtx.dist <- as.dist(1 - cor(data)) # matrix das distancias
  }
     
  hc <- hclust(mtx.dist, method = method) # procedimento hierarquico
     
  groups <- 0
  if (num.groups!=0) 
     groups <- cutree(hc, k = num.groups) # grupos formados

  if (analysis == "OBS") # novos grupos para as observacoes
     m.groups  <- cbind(data, groups) # matriz com dados originais mais os grupos formados
     
  if (analysis == "VAR") {# novos grupos para as variaveis
      m.groups <- cbind(colnames(data), groups) # matriz com dados originais mais os grupos formados
      colnames(m.groups) <- c("Variables", "groups")
      rownames(m.groups) <- NULL
  }
     
  ## INICIO - Tabelas com as Similaridade e as Distancias ##
  if (num.groups == 0) dist.groups <- hc$height else # Distancias dos agrupamentos
     dist.groups <- hc$height[(length(hc$height) - num.groups):length(hc$height)]

  sim.calc <- (1 - dist.groups / max(mtx.dist)) # calculo das similaridades
     
  steps      <- 1:length(sim.calc)
  sim.groups <- length(sim.calc):1
  similarity <- sim.calc * 100
  tab.sim    <- cbind(steps, sim.groups, round(similarity,3), round(dist.groups,2))
  colnames(tab.sim) <- c("Steps", "groups", "Similarity", "distance")
  ## FIM - Tabela com as similirades e distancias ##
     
  ## INICIO - Screen plots ##
  if (analysis == "OBS") {
        
     if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
       
     if (savptc) {
        name.figure = paste("Figure Cluster Screen plots 1 - distance ",distance," - method ", method,".png",sep="")
        png(filename = name.figure, width = width, height = height, res = res) # salva os graficos em arquivos
     }
       
     plot(length(sim.calc):1, 1/sim.calc, 
          type = "n", 
          xlab = "Number of clusters", 
          ylab = "Similarity within groups",
          main = titles[1]) # Titulo)
            
     ## Inicio - Grid
     args <- append(as.list(par('usr')), c('gray93','gray93'))
        
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
        
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
        
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
     ## Fim - Grid
        
     points(length(sim.calc):1, 1/sim.calc, lwd = 1.5, lty = 1, type = "b")
        
     abline(v = num.groups, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
     if (savptc) {
         box(col = 'white')
         dev.off()
     }
        
     if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
       
     if (savptc) {
        name.figure = paste("Figure Cluster Screen plots 2 - distance ",distance," - method ", method,".png",sep="")
        png(filename = name.figure, width = width, height = height, res = res) # salva os graficos em arquivos
     }
        
     plot(length(dist.groups):1, dist.groups, 
          type ="n", 
          xlab ="Number of clusters", 
          ylab ="Distances within groups",
          main = titles[2]) # Titulo
           
     ## Inicio - Grid
     args <- append(as.list(par('usr')), c('gray93','gray93'))
        
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
        
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
       
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
     ## Fim - Grid
        
     points(length(dist.groups):1, dist.groups, lwd = 1.5, lty = 1, type = "b")
        
     abline(v = num.groups, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
     if (savptc) {
        box(col = 'white')
        dev.off()
     }
        
  }
  ## FIM - Screen plots ##
 
  if (casc && !savptc) dev.new()  # efeito cascata na apresentacao dos graficos
     
  ## INICIO - Plotagem do Dendrograma ##
  if (savptc) {
     name.figure = paste("Figure Cluster Dendrogram - distance ",distance," - method ", method,".png",sep="")
     png(filename = name.figure, width = width, height = height, res = res) # salva os graficos em arquivos
  }
     
  dendro <- as.dendrogram(hc)
  plot(dendro, # cordenadas para plotar
      ylab   = "Distance",  # Nomeia Eixo Y
      main   = titles[3],   # Titulo
      center = TRUE,        # centraliza o grafico
      horiz  = horizontal,  # posicao do grafico
      #panel.first = grid(col = 'gray93', lwd = 2, lty = 1, equilogs = T),
      cex = 3) # Tamanho dos pontos
     
  if (num.groups > 1 && !horizontal) 
     rect.hclust(hc, k = num.groups, border = "red") # coloca retangulos nos agrupamentos de acordo com num.groups
     
  if (savptc) { 
     dev.off() 
     message("\n \n End!")
  }
  ## FIM - Plotagem do Dendrograma ##


  ### INICIO - analysiss dos grupos ###
  sum.sqt <- NA # soma do quadrado total 
  R.sqt   <- 0  # R quadrado
  tab.res.groups = NA # tabela com os resultados dos grupos
  if (analysis == "OBS" && num.groups > 1) {
     tab.res.groups <- NULL
     mtx.groups <- cbind(data.new, groups) # matriz com dados originais mais os grupos formados
     mean.g <- apply(data.new, 2, mean)
     SSB <- 0 # soma de quadrado entre grupos
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
  }
  ### FIM - analysiss dos grupos ###
   
  message("\n \n End!")
  
  lista <- list(tab.res = tab.sim, groups = m.groups, 
                res.groups = tab.res.groups, sum.sqt = sum.sqt,
                R.sqt = R.sqt, mtx.dist = mtx.dist)
  
  return(lista)
}