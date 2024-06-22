silhouette <- function(data, k.cluster = 2:10, plot = TRUE, cut = TRUE,
                  title = NA, xlabel = NA, ylabel = NA, size = 1.1, 
                  grid = TRUE, color = TRUE, savptc = FALSE, width = 3236, 
                  height = 2000, res = 300, casc = TRUE) {
  
  # Funcao criada para encontrar o numero ideal de clusters usando o metodo silhouette.
  # Devensolvida por Paulo Cesar Ossani em 21/06/2024.
  
  # Entrada:
  # data   - Matriz ou database dos dados, sem as classes.
  # k.cluster - Numeros de cluster para comparacao no metodo k-means (default = 2:10).
  # plot   - Indica se plota o grafico Elbow (default = TRUE).
  # cut    - Indica se plota a linha indicativa de melhor cluster (default = TRUE)
  # title  - Titulo para o grafico, se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos no grafico e a espessura da linha (default = 1.1)
  # grid   - Coloca grade no grafico (default = TRUE).
  # color  - Grafico colorido (default = TRUE).
  # savptc - Salva a imagen do grafico em arquivo (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # k.ideal - numero ideal de clusters
  # eve.si  - vetor com medias dos indices de si
  
  if (!is.data.frame(data) && !is.matrix(data))
     stop("'data' input is incorrect, it should be of type data frame. Verify!")

  test.integer <- function(x){
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
    if(test == TRUE){ return(TRUE) }
    else { return(FALSE) }
  }  
  if (!test.integer(k.cluster))
     stop("'k.cluster' imput is incorrect, it should be an integer. Verify!")

  k.mim <- min(k.cluster)
  if (k.mim < 2) 
     stop("'k.cluster' imput is incorrect, it should be an integer greater than one. Verify!")
  
  k.max <- max(k.cluster)
  if (k.max >= nrow(data)) 
     stop("'k.cluster' imput is incorrect, it should be an integer smaller than the number of observations in 'data'. Verify!")
  
  if (!is.logical(cut))
     stop("'cut' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(plot))
     stop("'plot' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.numeric(size) || size <= 0)
     stop("'size' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(grid))
     stop("'grid' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(color))
     stop("'color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(casc) && !savptc)
     stop("'casc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  k.cluster <- sort(k.cluster) # ordena os dados
  
  num.groups <- length(k.cluster) # numero de elementos de clusters 
  
  if (!is.character(title) || is.na(title)) {
    if (num.groups == 1) { # caso haja apenas um elemento em k.cluster
       title = "Silhouette plot"
    } else title = "Silhouette index plot" 
  } 
  
  if (is.na(xlabel[1]))
     xlabel = "Number of clusters"
  
  if (is.na(ylabel[1]))
     ylabel = "Average silhouette width"
  
  set.seed(7) # semente, numero fixo para a eleatoriazacao
   
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end! \n")
 
  set.seed(7) # semente para fixar processo heuristico
  
  dist <- "euclidean"
  mat.dist <- as.matrix(stats::dist(data, method = dist)) # matrix com as distancias
  
  ave.si <- rep(0,num.groups) # vetor com as medias das silhuetas dos clusters
  j.si   <- 2 # auxiliar como contador da posicao em eve.si
  for(k in k.cluster) {

    if(num.groups > 1) { # tempo inicial do processamento
      time.now <- Sys.time(); start.time <- as.numeric(time.now) 
    } 
    
    hc <- stats::kmeans(data, k, iter.max = 100) # executa o method k-means

    clusters <- hc$cluster # vetor com os clusters encontrados
    
    rm(hc) # limpa memoria
    
    ### Inicio - analises dos clusters ###
    class.table <- table(clusters) # cria tabela com as quantidade dos elementos das classes
    class.names <- as.numeric(names(class.table)) # nomes das classses
    mean.si <- rep(0,k) # medias das distancias no clusters
    mtx.si  <- NULL # matriz com nomes dos clusters e os coeficientes de silhuetas - usado no plot de silhuetas
    for(i in 1:k) {
     
      if (class.table[i] > 1) {
        
         xy <- which(clusters == i) # filtra o i-ésimo cluster para usar na matriz de distancias

         # Calcular a distância media para todos os outros pontos no mesmo cluster (a_i)
         ai <- colMeans(mat.dist[xy,xy])
         
         ## Inicio - encontra as medias das distancias em relacao aos outros clusters e escolhe aqueles com a menor ditancia ##
         new.table <- class.table[-i]   
         names     <- names(new.table)  # nomes das classes restantes
         k.new     <- length(new.table) # numero de classes restantes
         mtx.mean  <- NULL # matriz com as medias das distancias nos clusters
         for(j in 1:k.new) {
   
            x.y <- which(clusters == names[j]) # filtra outro cluster
          
            mean <- mat.dist[x.y,xy]
            if (!is.vector(mean)) { # se for uma matriz
               mean <- colMeans(mat.dist[x.y,xy])
            }
            
            mtx.mean <- rbind(mtx.mean, mean) # medias das distancias nos clusters
          
         }
         
         min.bi <- apply(mtx.mean, 2, which.min) # escolhe os itens com a menor distancia
         
         # Calcular a distância media para todos os pontos em cada cluster vizinho mais proximo (b_i)
         bi <- mtx.mean[cbind(min.bi, seq(1:class.table[i]))] # itens com a menor distancia
         
         # ### Modo mais rapido para encontra bi
         # # encontra as medias das distancias nos clusters 
         # mat.bi <- rbind(apply(mat.dist[!xy, xy, drop = FALSE], 2, function(x) tapply(x, clusters[!xy], mean)))
         # min.bi <- apply(mat.bi, 2, which.min) # escolhe os itens com a menor distancia
         # bi <- mat.bi[cbind(min.bi, seq(1:class.table[i]))] # itens com a menor distancia
         
         si <- ifelse(ai != bi, (bi - ai) / pmax(bi,ai), 0)
         
      } else si <- 0
      ## Fim - encontra as medias das distancias em relacao aos outros clusters e escolhe aqueles com a menor ditancia ##
      
      mtx.si <- rbind(mtx.si, cbind(as.data.frame(class.names[i]), sort(si, decreasing = TRUE)))
      
      mean.si[i] <- mean(si)
      
    }
    rownames(mtx.si) <- NULL
    colnames(mtx.si) <- c("clusters","IndexSi")
    
    ave.si[j.si] <- mean(mean.si) # vetor com as medias das silhuetas dos clusters 
    j.si <- j.si + 1
    ### Fim - analises dos clusters ###
    
    if (num.groups > 1) {
       time.now <- Sys.time(); end.time <- as.numeric(time.now) # tempo final do processamento
         
       time.left <- (end.time - start.time) * (num.groups - k + 1) # tempo de processamento
       time <- paste("days:", round(time.left / 86400, 2), 
                     "hours:", round(time.left / 3600, 2),
                     "minutes:", round(time.left / 60, 2))
        
       print(paste("Processing k clusters: ", k, "/", k.max, " Estimated time to finish: ", time, sep = ""))
    }
  }

  pos.k <- k.cluster[which.max(ave.si)-1]
  
  if (num.groups > 1 && pos.k < k.mim || num.groups == 1) pos.k <- NA
  
  if(plot && num.groups > 1) { 
    
    ## Inicio - Grafico dos indices Si ##
    if (savptc) png(filename = "Silhouette Index graph.png", width = width, height = height, res = res) # salva os graficos em arquivo
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    axis.x <- c(1, k.cluster) # eixo X
    
    plot(axis.x, ave.si,
         type = "n",    # nao plota pontos
         # asp = -1,
         main = title,  # Titulo
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         xaxt = "n")    # tira o eixo x

    axis(1, axis.x)
    
    if (grid) {
    
        args <- append(as.list(par('usr')), c('gray93','gray93'))
        
        names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
        
        do.call(rect, args) # chama a funcao rect com os argumentos (args)
        
        grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
    }
    
    col <- ifelse(color, "blue", "Black")
    
    points(axis.x,ave.si,   # coordenadas do grafico
           type = "o",
           pch  = 16,       # formato dos pontos
           lwd  = size+0.5, # espessura da linha
           cex  = size,     # Tamanho dos pontos
           col  = col)

    if(pos.k > 1 && cut && !is.na(pos.k)) abline(h = 1, v = pos.k, cex = 1.5, lty = 2) # coloca o corte no grafico
  
  }
  ## Inicio - Grafico dos indices Si ##
  
  ## Inicio - Grafico de Silhueta ##
  if (num.groups == 1) { # caso haja apenas uma entrada em k.cluster 
    
     if (savptc) png(filename = "Silhouette graph.png", width = width, height = height, res = res) # salva os graficos em arquivo
    
     if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
     ## Inicio - Ordena os dados em mtx.si dentro das classes ##
     clts <- mtx.si[,"clusters"]
     mtx.si <- mtx.si[order(clts, -mtx.si[,"IndexSi"]), , drop = FALSE]
     ## Fim - Ordena os dados em mtx.si dentro das classes ##
    
     ind.si <- rev(mtx.si[,"IndexSi"])
     
     ## lacuna entre os clusters
     spc <- c(0, rev(diff(as.numeric(mtx.si[,"clusters"]))))
     spc[spc != 0] <- 0.5 
     
     col <- ifelse(color, "blue", "gray")
     
     xlab = expression("Silhouette width " * s[i])
    
     y.res <- barplot(ind.si, space = spc, names = NA, xlab = xlab,
                      xlim = c(min(0, min(ind.si)), 1), horiz = TRUE,
                      las = 1, mgp = c(2.5, 1, 0), col = col, border = 0)
     
     sub  <- paste("Average silhouette width : ", format(round(mean(mean.si),2), nsmall = 2))
     title(main = title, adj = 0.5)
     title(sub = sub, adj = 0) 
     
     mtext(paste("Observations: ", nrow(mtx.si)),	adj = 0.0)
     mtext(substitute(k ~~ "clusters" ~~ C[j], list(k = k)), adj = 1.0)
     mtext(expression(paste(j," :  ", n," | ", ave[i %in% Cj] ~~ s[i])), adj = 1.03, line = -1.0)
     
     y.res <- rev(y.res)
     for(j in 1:k) {
        y.mean <- mean(y.res[mtx.si[,1] == j])
        text(1, y.mean, paste(j,":  ", class.table[j]," | ", format(round(mean.si[j],2), nsmall = 2)), xpd = NA, adj = 0.9)
     }
      
  }
  ## Fim - Grafico de Silhueta ##
  
  if (savptc) { 
     box(col = 'white')
     dev.off()
     message("\n Saved graphic. End!")
  } else message("\n End!")

  lista <- list(k.ideal = pos.k, eve.si = ave.si[-1])
  
  return(lista)
}

# ave.si <- c(0,0.55,0.67,0.51,0.53,0.45,0.46,0.39,0.31,0.32)