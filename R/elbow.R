elbow <- function(data, k.max = 10, method = "AutoElbow", plot = TRUE, cut = TRUE,
                  title = NA, xlabel = NA, ylabel = NA, size = 1.1, 
                  grid = TRUE, color = TRUE, savptc = FALSE, width = 3236, 
                  height = 2000, res = 300, casc = TRUE) {
  
  # Funcao criada para encontrar o numero ideal de clusters usando o metodo elbow.
  # Devensolvida por Paulo Cesar Ossani em 03/05/2024.
  
  # Entrada:
  # data   - Matriz ou database dos dados, sem as classes.
  # k.max  - Numero maximo de clustes para comparacao (default = 10).
  # method - Metodo usado para encontrar o numero k ideal de agrupamentos:
  #          'jump', 'curvature', 'Exp', 'AutoElbow' (default).
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
  
  if (!is.data.frame(data) && !is.matrix(data))
     stop("'data' input is incorrect, it should be of type data frame. Verify!")
  
  if (k.max < 2 && !is.integer(k.max))
     stop("'k.max' imput is incorrect, it should be an integer greater than one. Verify!")
  
  method <- toupper(method) # transforma em maiusculo
  if      (method == "JUM") method = "JUMP" 
  else if (method == "CUR") method = "CURVATURE"
  else if (method == "AUT") method = "AUTOELBOW"
  if (!(method %in% c("JUMP", "CURVATURE", "AUTOELBOW", "EXP")))
     stop("'method' input is incorrect, it should be: 'jump', 'curvature', 'AutoElbow' or 'Exp'. Verify!")
  
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
  
  if (!is.character(title) || is.na(title)) title = c("Elbow graph")

  if (is.na(xlabel[1]))
     xlabel = "Number of clusters"
  
  if (is.na(ylabel[1]))
     ylabel = "Proportional variance"
  
  set.seed(7) # semente, numero fixo para a eleatoriazacao
   
  message("\014") # limpa a tela
  message("\n\n Processing the data. Wait for the end! \n")
  
  data <- scale(data) # normaliza por colunas os dados
  
  sum.sqt <- sum(data^2) # soma do quadrado total
  
  set.seed(7) # semente para fixar processo heuristico
  
  var.prop <- rep(1,k.max) # variancias proporcionais dos k clusters
  for(num.clusters in 2:k.max) {
    
    time.now <- Sys.time(); start.time <- as.numeric(time.now) # tempo inicial do processamento
    
    hc <- stats::kmeans(data, num.clusters, iter.max = 100) # executa o method K-Means

    clusters <- hc$cluster # vetor com os clusters encontrados
    
    rm(hc) # limpa memoria
    
    ### Inicio - analises dos clusters ###
    # mtx.clusters <- cbind(as.data.frame(data), clusters) # matriz com dados originais mais os clusters formados
    sqt.k.clusters <- 0 # soma de quadrado total dos k clusters
    for (i in 1:num.clusters) { 
      # new.data  <- mtx.clusters[clusters == i, 1:ncol(data)]
      new.data <- data[clusters == i, ]
      # sqt.k.clusters  <- sqt.k.clusters + sum(sweep(new.data, 2, apply(new.data , 2, mean))^2)  # soma de quadrado total dos k clusters
      sqt.k.clusters <- sqt.k.clusters + sum(scale(new.data, center = TRUE, scale = FALSE)^2)  # soma de quadrado total dos k clusters
      
      rm(new.data) # limpa memoria
    }
    
    var.prop[num.clusters] <- sqt.k.clusters / sum.sqt 
    ### Fim - analises dos clusters ###
    
    time.now <- Sys.time(); end.time <- as.numeric(time.now) # tempo final do processamento
    
    time.left <- (end.time - start.time) * (k.max - num.clusters) # tempo de processamento
    time <- paste("days:", round(time.left / 86400, 2), 
                  "hours:", round(time.left / 3600, 2),
                  "minutes:", round(time.left / 60, 2))
    
    print(paste("Processing k clusters: ", num.clusters, "/", k.max, " Estimated time to finish: ", time, sep = ""))
  }
  
  ### Inicio - Encontra o melhor k ###
  k.cluster <- rep(0,length(var.prop))
  if (method == "JUMP") { # Sugar e James (2003)
    
    # var <- var.prop #^(-ncol(data) / 2)
    for(i in 3:length(var.prop))
      k.cluster[i] <- abs(var.prop[i] - var.prop[i-1])
    
  } else if (method == "CURVATURE") { # Zhang et al.(2017) 
    
    for(i in 3:(length(var.prop)-1))
      k.cluster[i] <- (var.prop[i-1] - var.prop[i]) / (var.prop[i] - var.prop[i+1]) # - 1
    
  } else if (method == "AUTOELBOW") { # Onumanyi et al. (2022) 
    
    x <- 1:k.max
    y <- var.prop
    
    x.k <- (x - min(x)) / (max(x) - min(x)) # normalizado
    y.k <- (y - min(y)) / (max(y) - min(y)) # normalizado
    
    k.cluster <- ((x.k - 1)^2 + (y.k - 1)^2) / (x.k^2 + 2*y.k^2)
    
  } else if (method == "EXP") { # Exponencial Ossani (2024)
    
    x <- 1:length(var.prop)
    y <- var.prop
    
    reg  <- stats::lm(log(y)~x)
    
    pred <- predict(reg, as.data.frame(x)) 
    
    k.cluster <- y - pred
    
  } 
  
  if (method == "EXP") {
    pos.k <- which.min(k.cluster)
  } else { 
    pos.k <- which.max(k.cluster)
  }
  
  # pos.k <- 0
  # for(r in 2:length(var.prop)) { # encontra o melhor k
  #   if((var.prop[r-1] - var.prop[r]) < 0.05) { # mean(var.prop[2:length(var.prop)])) {
  #     pos.k <- r - 1
  #     break
  #   }
  # }
  # 
  # if(pos.k == 1) {
  #   for(w in 2:length(var.prop)) { # encontra onde ha um salto no grafico
  #     if(var.prop[w-1] < var.prop[w]) {
  #       pos.k <- w
  #       break
  #     }
  #   }
  # }
  ### Fim - Encontra o melhor k ###

  if(plot) {
    
    if (savptc) png(filename = "Elbow_graph.png", width = width, height = height, res = res) # salva os graficos em arquivo
    
    if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
    
    plot(1:k.max, var.prop,
         type = "n",    # nao plota pontos
         # asp = -1,
         main = title,  # Titulo
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         xaxt = "n")    # tira o eixo x

    axis(1, 1:k.max)
    
    if (grid) {
    
        args <- append(as.list(par('usr')), c('gray93','gray93'))
        
        names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
        
        do.call(rect, args) # chama a funcao rect com os argumentos (args)
        
        grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
    }
    
    cor <- ifelse(color, "blue", "Black")
    
    points(1:k.max, var.prop,   # coordenadas do grafico
           type = "o",
           pch  = 16,       # formato dos pontos
           lwd  = size+0.5, # espessura da linha
           cex  = size,     # Tamanho dos pontos
           col  = cor)
    
    if(pos.k > 1 && cut) abline(h = 0, v = pos.k, cex = 1.5, lty = 2) # coloca o corte no grafico
    
  }
  
  if (savptc) { 
     box(col = 'white')
     dev.off()
     message("\n Saved graphic. End!")
  } else message("\n End!")

  lista <- list(k.ideal = pos.k)
  
  return(lista)
}

# var.prop = c(1,0.75,0.55,0.4,0.39,0.385,0.38,0.375,0.37,0.365) # figura 1
# var.prop = c(1, 0.78, 0.65, 0.57, 0.53, 0.515, 0.505, 0.5, 0.495, 0.49) # figura 2
