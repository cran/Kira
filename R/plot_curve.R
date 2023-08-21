plot_curve <- function(data, type = "ROC", title = NA, xlabel = NA, ylabel = NA,  
                posleg = 3, boxleg = FALSE, axis = TRUE, size = 1.1, grid = TRUE, 
                color = TRUE, classcolor = NA, savptc = FALSE, width = 3236, 
                height = 2000, res = 300) { 

  # Esta funcao plota a curva ROC e PRC desenvolvida 
  # por Paulo Cesar Ossani em 27/05/2021
  
  # Entrada:
  # data   - Dados com as coordenadas x e y.
  # type   - Tipo de grafico ROC (default) ou PRC.
  # title  - Titulo do grafico, se nulo retorna padrao.  
  # xlabel - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel - Nomeia o eixo Y, se nulo retorna padrao. 
  # posleg - 0 sem legenda,
  #          1 para legenda no canto superior esquerdo,
  #          2 para legenda no canto superior direito (default),
  #          3 para legenda no canto inferior direito,
  #          4 para legenda no canto inferior esquerdo.  
  # boxleg - Colocar moldura na legenda (default = TRUE).  
  # axis   - Coloca o eixo diagonal no grafico (default = TRUE).
  # size   - Tamanho dos pontos no grafico (default size = 1.1).
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # classcolor - Vetor com as cores das classes.
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).

  # Retorna:
  # curva ROC ou PRC.

  type <- toupper(type) 
  
  if(!(type %in% c("ROC","PRC")))
     stop("'type' input is incorrect, it should be 'ROC' or 'PRC'. Verify!")
  
  if (!is.logical(axis)) 
     stop("'axis' input is incorrect, it should be TRUE or FALSE. Verify!")
 
  if (!is.character(title) && !is.na(title[1]))
     stop("'title' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("'posleg' input is incorrect, it should be a integer number between [0,4]. Verify!")
  
  if (!is.logical(grid))
     stop("'grid' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(size) || size < 0)
     stop("'size' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.logical(color))
     stop("'color' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.logical(boxleg)) 
     stop("'boxleg' input is incorrect, it should be TRUE or FALSE. Verify!")
  
   if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("'xlabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("'ylabel' input is incorrect, it should be of type character or string. Verify!")
  
  if (!is.logical(savptc))
     stop("'savptc' input is incorrect, it should be TRUE or FALSE. Verify!")
  
  if (!is.numeric(width) || width <= 0)
     stop("'width' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(height) || height <= 0)
     stop("'height' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  if (!is.numeric(res) || res <= 0)
     stop("'res' input is incorrect, it should be numerical and greater than zero. Verify!")
  
  ### Inicio - Informacoes usadas nos Graficos ###
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Saving graphics to hard disk. Wait for the end!")
  }

  if (type == "ROC") { # curva ROC
     if (is.na(title[1]))  title  = "ROC curve"
     if (is.na(xlabel[1])) xlabel = "False positive rate (1 - specificity)"  
     if (is.na(ylabel[1])) ylabel = "True positive rate (sensitivity)"
  }
  
  if (type == "PRC") { # curva PRC
     if (is.na(title[1]))  title  = "PRC curve"
     if (is.na(xlabel[1])) xlabel = "Recall"
     if (is.na(ylabel[1])) ylabel = "Precision" 
  }
  
  if (posleg == 1) posleg = "topleft"     # posicao das legendas nos graficos
  if (posleg == 2) posleg = "topright"
  if (posleg == 3) posleg = "bottomright"
  if (posleg == 4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  class <- data[,1] # classes
  
  names.class <- as.character(unique(class)) # nomes das classes
  num.class   <- length(names.class) # numero de classes

  if (num.class != 0 && length(classcolor) != num.class && !is.na(classcolor) ||
      num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
      stop("'classcolor' input is incorrect, it should be in an amount equal to the number of classes in 'class'. Verify!")
  ### Fim - Informacoes usadas nos Graficos ###

  ### Inicio - Plota uma classe por vez ### 
  init.form <- 14 # formato inicial dos pontos
  
  cor <- ifelse(color, 2, 1)
  
  if (color) {
    if (!is.na(classcolor[1])) {
      cor <- classcolor
    }
    else { cor <- cor:(cor + num.class - 1) }
  }
  
  for (i in 1:num.class) {
    
    dat <- data[data == names.class[i],]
    
    x <- dat[,2] # valores eixo x
    y <- dat[,3] # valores eixo y
    
    if (savptc) png(filename = paste("Figure ", type, " curve - ", names.class[i],".png", sep =""), width = width, height = height, res = res) # salva os graficos em arquivos
    
    point.form <- init.form + i # fomato dos pontos de cada classe
    
    plot(.5,.5, # cria grafico para as coordenadas linhas x e colunas y
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = title,  # Titulo
         asp  = 0,      # Aspecto do Grafico
         axes = FALSE,  # Eixos x e y
         type = "n",    # nao plota pontos
         xlim = c(0.02,0.98), # Dimensao para as linhas do grafico
         ylim = c(0.02,0.98)) # Dimensao para as colunas do grafico 
    
    sqc <- seq(0,1, by = 0.1)
    
    axis(1, at = sqc)
    axis(2, at = sqc)
    
    if (grid) {
      
       args <- append(as.list(par('usr')), c('gray93','gray93'))
      
       names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
      
       do.call(rect, args) # chama a funcao rect com os argumentos (args)
      
       grid(col = "white", lwd = 2, lty = 7, equilogs = T)
      
       for(k in 1:length(sqc))
          abline(h = sqc[k], v = sqc[k], lwd = 2, lty = 7, col = "white")
      
    }
    
    if (axis && type == "ROC") # coloca eixo diagonal no grafico
       lines(x = c(0,1), y = c(0,1), cex = 1.5, lty = 2)
    
    lines(x, y, cex = 1.5, lty = 2, col = ifelse(color, cor[i], "Black"))
    
    points(x, y, # cria grafico para as coordenadas principais das linhas
           pch = point.form, # Formato dos pontos 
           cex = size, # Tamanho dos pontos  
           col = ifelse(color, cor[i], "Black"))
    
    box(col = 'black')

    if (posleg != 0) {
       legend(posleg, names.class[i], pch = (init.form + 1 +i), col = cor[i],
              text.col = cor[i], bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
      
    }
    
    if (savptc) dev.off()
  }
  ### Fim - Plota uma classe por vez ### 
  
  ### Inicio - Plota todas as classes conjuntamente ### 
  if (savptc) png(filename = paste("Figure", type, "curve - all classes.png"), width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(.5,.5, # cria grafico para as coordenadas linhas x e colunas y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = title,  # Titulo
       asp  = 0,      # Aspecto do Grafico
       axes = FALSE,  # Eixos x e y
       type = "n",    # nao plota pontos
       xlim = c(0.02,0.98), # Dimensao para as linhas do grafico
       ylim = c(0.02,0.98)) # Dimensao para as colunas do grafico 
  
  sqc <- seq(0,1, by = 0.1)
  
  axis(1, at = sqc)
  axis(2, at = sqc)
  
  if (grid) {
    
    args <- append(as.list(par('usr')), c('gray93','gray93'))
    
    names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
    do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
    grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
    for(k in 1:length(sqc))
       abline(h = sqc[k], v = sqc[k], lwd = 2, lty = 7, col = "white")
    
  }
  
  if (axis && type == "ROC") # coloca eixo diagonal no grafico
     lines(x = c(0,1), y = c(0,1), cex = 1.5, lty = 2)
  
  for (i in 1:num.class) {
    
    point.form <- init.form + i # fomato dos pontos de cada classe
    
    dat <- data[data == names.class[i],]
    
    x <- dat[,2] # valores eixo x
    y <- dat[,3] # valores eixo y
    
    lines(x, y, cex = 1.5, lty = 2, col = ifelse(color, cor[i], "Black"))
    
    points(x, y, # cria grafico para as coordenadas principais das linhas
           pch = point.form, # Formato dos pontos 
           cex = size, # Tamanho dos pontos  
           col = ifelse(color, cor[i], "Black"))
  }
  
  box(col = 'black')
  
  if (posleg != 0) {
     legend(posleg, names.class, pch = (init.form+1):(init.form + num.class), col = cor,
            text.col = cor, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
    
  }
  
  if (savptc) {
     dev.off()
     message("\n \n End!")
  }
  ### Fim - Plota todas as classes conjuntamente ### 
  
}
