
# ---- Función de filtrado de outliers ----
filter.outliers <- function(df, filter.type, sel.col, h, u, window.size=NA){
  outliers <- c() # inicializar outliers (shiny protesta si no se hace)
  if (is.na(window.size)){
    window.size <- 5
  }
  
  if (is.na(filter.type)){
    return(df)
    
  } else if (filter.type=='boxplot'){
    # -- Método simple (por boxplot) --
    outliers <- boxplot(df[[sel.col]], plot=FALSE)$out
    
  } else if (filter.type=='extended'){
    # -- Método más refinado (boxplot y quedarse con los que no se separen más de un 10% de los valores del Q1 y Q3) --
    outliers <- boxplot(df[[sel.col]], plot=FALSE)$out
    
    # Calcular los límites típicos para que un valor sea considerado outlier
    quantiles <- quantile(df[[sel.col]], probs=c(0.1, 0.9), na.rm = TRUE)
    iqr <- IQR(df[[sel.col]], na.rm=TRUE)
    low <- quantiles[1]-1.5*iqr
    up <-  quantiles[2]+1.5*iqr  
    
    # Calcular el límite extendido (límite + límite*porcentaje)
    extra.pct <- 0.15
    lower <- low-low*extra.pct
    upper <- up+up*extra.pct
    
    # Filtrar los outliers
    cond <- outliers >= lower & outliers <= upper # ver cuáles se encuentran en el rango extendido de lower y upper
    outliers <- outliers[!cond] # quitar los que se encuentran en ese rango
    
  } else if (filter.type=='sliding_median'){
    df[[sel.col]] <- rollapply(df[[sel.col]], width=window.size, FUN=median, align='left', fill=NA, partial=TRUE)
    return(df)
    
  } else if (filter.type=='sliding_mean'){
    df[[sel.col]] <- rollapply(df[[sel.col]], width=window.size, FUN=mean, align='left', fill=NA, partial=TRUE)
    return(df)
  }
  
  # Eliminar outliers
  if (length(outliers)!=0){ # si hay algún outlier
    df <- df[-which(df[[sel.col]] %in% outliers),] # quitar las filas del dataset con los que hayan sido outliers
  }
  
  # print(glue('Se eliminan {length(outliers)} outliers de {sel.col} ({h} - {u})\n'))
  return(df)
  
}
