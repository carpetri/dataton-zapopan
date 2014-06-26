library(ProjectTemplate)
reload.project()
library(gstat)
library(geoR)
ls()


#GEOESPACIAL
agebs.jalisco <- readShapeSpatial("data/inegi-jalisco/jal_ageb_urb.shp")
agebs.jalisco.fuerte <- fortify(agebs.jalisco)
head(agebs.jalisco.fuerte)

mun.jalisco <- readShapeSpatial("data/inegi-jalisco/jal_municipal.shp")
zapopan <- mun.jalisco[which(mun.jalisco$CVEGEO==14120),]
mun.jalisco.fuerte <- fortify(mun.jalisco)
zapopan.fuerte <- fortify(zapopan) 
head(mun.jalisco.fuerte)


delitos.1 <- filter( delitos, x >0)
delitos.detenidos.1 <- filter(delitos.detenidos, x>0 )

delitos.detenidos.1$tipo <- gsub(delitos.detenidos.1$tipo, pattern = 'FALTAS ADMINISTRATIVAS', 
     replacement = 'FALTA ADMINISTRATIVA')

ggplot(data = jalisco.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 5/3.5)+  labs(title = "AGEBS en Jalisco", x = "Longitud", y = "Latitud", 
       colour = "Tipo de delito", size = "", alpha = "")  + 
   geom_point(data = delitos.1, aes(x = long, y = lat, colour = tipo),
              size = 1.5, alpha = 0.9) 



ggplot(data = mun.jalisco.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Municipios en Jalisco", x = "Longitud", y = "Latitud", 
                                    colour = "Tipo de delito", size = "", alpha = "")  + 
  geom_point(data = delitos.1, aes(x = long, y = lat, colour = tipo),
             size = 1.5, alpha = 0.9) 
ggsave(filename = 'graphs/munucipios_y_delitos_jalisco.pdf', width = 9, height = 6 )


ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                    colour = "Tipo de delito", size = "", alpha = "")  + 
  geom_point(data = delitos.1, aes(x = long, y = lat, colour = tipo),
             size = 2, alpha = 1) 
ggsave(filename = 'graphs/zapopan_delitos.pdf', width = 9, height = 6 )

ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos detenidos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Tipo de delito", size = "", alpha = "")  + 
  geom_point(data = delitos.detenidos.1, aes(x = long, y = lat, colour = tipo),
             size = 2, alpha = 1) 
ggsave(filename = 'graphs/zapopan_delitos_detenidos.pdf', width = 9, height = 6 )

tabla.1 <- data.frame( delitos_cometidos = table(delitos.1$tipo),delitos_detenidos=table(delitos.detenidos.1$tipo))
tabla.1$delitos_detenidos.Var1 <- NULL

names(tabla.1) <- c('Tipo de delito', 'Cometidos', 'Detenidos')

print(xtable(tabla.1, caption = 'Número de delitos cometidos y detenidos'),include.rownames=FALSE )


tabla.2 <- data.frame( delitos_cometidos = round(100*prop.table(table(delitos.1$tipo)), digits = 0),
                       delitos_detenidos= round(100*prop.table(table(delitos.detenidos.1$tipo)) ) )
tabla.2$delitos_detenidos.Var1 <- NULL

names(tabla.2) <- c('Tipo de delito', '% de cometidos', '% de detenidos')
rownames(tabla.2) <- NULL

print(xtable(tabla.2, caption = 'Porcentaje de delitos cometidos y detenidos', digits = 0),include.rownames=FALSE )



