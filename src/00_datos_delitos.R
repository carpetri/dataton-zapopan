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

# ggplot(data = jalisco.fuerte, aes(long, lat)) + 
#   geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
#   coord_fixed(ratio = 5/3.5)+  labs(title = "AGEBS en Jalisco", x = "Longitud", y = "Latitud", 
#        colour = "Tipo de delito", size = "", alpha = "")  + 
#    geom_point(data = delitos.1, aes(x = long, y = lat, colour = tipo),
#               size = 1.5, alpha = 0.9) 

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

print(xtable(tabla.2, caption = 'Porcentaje de delitos cometidos y detenidos', 
             digits = 0),include.rownames=FALSE )


# tabla.3 <- data.frame( delitos.por.colonia= head(sort(table(delitos.1$colonia), decreasing = T), 20) )
# names(tabla.3) <- 'Número de delitos por colonia'
# 
# print(xtable(tabla.3, caption = 'Las 20 colonias con más delitos', 
#              digits = 0),include.rownames=T)

delitos.colonia <- as.data.frame(table(delitos.1$colonia , delitos.1$tipo))
names(delitos.colonia) <- c('Colonia','Tipo de delito','n')

xtable(head(arrange(delitos.colonia, -n), 20)  , caption='Las 20 colonias más peligrosas')


#MAPAS
head(delitos.colonia)



manz.zapopan <- readShapeSpatial("data/shp_zapopan/zapopan_manzanas.shp")

manz.zapopan.fuerte <- fortify(manz.zapopan)
zapopan.fuerte <- fortify(zapopan) 
head(mun.jalisco.fuerte)


coord.colonias <- delitos.1 %.% group_by(colonia,tipo) %.% summarize( long= mean(long), lat=mean(lat),
                                                                 n=n())

coord.colonias

ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Tipo de delito", size = "Número de delitos", alpha = "")  + 
  geom_point(data = coord.colonias  , aes(x = long, y = lat, size = n, colour=tipo), alpha = 1) 
ggsave(filename = 'graphs/num_delitos.pdf', width = 9, height = 6 )



ggplot(data = manz.zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Tipo de delito", size = "Número de delitos", alpha = "")  + 
  geom_point(data = coord.colonias  , aes(x = long, y = lat, size = n, colour=tipo), alpha = 1) 
ggsave(filename = 'graphs/num_delitos_manz.pdf', width = 9, height = 6 )

#MESES y fechas
delitos.1$fecha.1  <- as.Date(delitos.1$fecha , format="%d/%m/%Y")

delitos.1$mes <-(month(delitos.1$fecha.1))
delitos.1$fecha
#delitos.1 <- tbl_df(delitos.1)


delitos.mes <-  delitos.1 %.% group_by(colonia,mes) %.% summarise( long= mean(long), lat=mean(lat),n=n())

x <- delitos.1 %.% group_by(mes) %.% summarise( long= mean(long), lat=mean(lat),n=n() ) %.%
  arrange(as.numeric(mes))
x
print(xtable(x[,c('mes','n')],caption='Número de delitos por mes en Zapopan'), include.rownames=FALSE )


ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Mes del delito", size = "Número de delitos", alpha = "")  + 
  geom_point(data =delitos.mes  , aes(x = long, y = lat, size = n, colour=factor(mes)), alpha = .8) 
ggsave(filename = 'graphs/num_delitos_mes.pdf', width = 9, height = 6 )

head(TemperaturasEneMar2014)

temps <- TemperaturasEneMar2014
temps$fecha  <- gsub(temps$Fecha, pattern = ' 12:00:00 AM', replacement = '')

temps$fecha.1 <- as.Date(temps$fecha, format='%m/%d/%Y')

temps.1 <- temps[,c('TMedia','fecha.1')]

temps.2 <- temps.1 %.% group_by(fecha.1) %.% summarise(TMedia=mean(TMedia) )

filter(temps.2,fecha.1=='2014-03-18')

delitos.2 <- left_join(delitos.1, temps.2)
nrow(delitos.1)
nrow(delitos.2)




