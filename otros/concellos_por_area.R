concellos.archivos <- list.files('datos/concellos_area', full.names=T)

coruña <- as.data.frame(fread(concellos.archivos[1], header=F))
ferrol <- as.data.frame(fread(concellos.archivos[2], header=F))
lugo <- as.data.frame(fread(concellos.archivos[3], header=F))
ourense <- as.data.frame(fread(concellos.archivos[4], header=F))
pontevedra <- as.data.frame(fread(concellos.archivos[5], header=F))
santiago <- as.data.frame(fread(concellos.archivos[6], header=F))
vigo <- as.data.frame(fread(concellos.archivos[7], header=F))


create.df <- function(concellos, nombre_area){
  tmp <- data.frame(area=nombre_area, concello=t(concellos))  
  return(tmp)
}

coruña.df <- create.df(coruña, "Coruña - Cee")
ferrol.df <- create.df(ferrol, "Ferrol")
lugo.df <- create.df(lugo, "Lugo - A Mariña - Monforte de Lemos")
ourense.df <- create.df(ourense, "Ourense - Verín - O Barco de Valdeorras")
pontevedra.df <- create.df(pontevedra, "Pontevedra - O Salnés")
santiago.df <- create.df(santiago, "Santiago de Compostela - Barbanza")
vigo.df <- create.df(vigo, "Vigo")


full.corr <- rbind(coruña.df, ferrol.df, lugo.df, ourense.df, pontevedra.df, santiago.df, vigo.df)

write.csv(full.corr,"datos/areas_concellos_correspondencia.csv", row.names = FALSE)




str_split()

for (c in full.corr$concello){
  cat(c, charmatch(c,unlist(str_split(unique(casos.org$areamovilidad),'y'))), '\n')
}

