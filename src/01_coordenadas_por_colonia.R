library(ProjectTemplate)
reload.project()


delitos.1 <- filter( delitos, x >0)
delitos.detenidos.1 <- filter(delitos.detenidos, x>0 )

delitos.detenidos.1$tipo <- gsub(delitos.detenidos.1$tipo, pattern = 'FALTAS ADMINISTRATIVAS', 
                                 replacement = 'FALTA ADMINISTRATIVA')


coord.colonias <- delitos.1 %.% group_by(colonia) %.% summarise( long= mean(long), lat=mean(lat),
                                                                 n=n())


#write.csv(coord.colonias,'data/coordenas_por_colonia.csv')


delitos.1$fecha.1 <- as.Date(delitos.1$fecha, format="%d/%m/%Y")

delitos.1$fecha.1
delitos.1$mes <- month(delitos.1$fecha.1)

quantile(heatvals2$val)

mun.jalisco <- readShapeSpatial("data/inegi-jalisco/jal_municipal.shp")
zapopan <- mun.jalisco[which(mun.jalisco$CVEGEO==14120),]
#mun.jalisco.fuerte <- fortify(mun.jalisco)
zapopan.fuerte <- fortify(zapopan) 
head(mun.jalisco.fuerte)


head(TemperaturasEneMar2014)

temps <- TemperaturasEneMar2014
temps$fecha  <- gsub(temps$Fecha, pattern = ' 12:00:00 AM', replacement = '')

temps$fecha.1 <- as.Date(temps$fecha, format='%m/%d/%Y')

temps.1 <- temps[,c('TMedia','fecha.1')]

temps.2 <- temps.1 %.% group_by(fecha.1) %.% summarise(TMedia=mean(TMedia) )

delitos.2 <- left_join(delitos.1, temps.2)
nrow(delitos.1)
nrow(delitos.2)

faltan <- unique( filter(delitos.2, is.na(TMedia) )$fecha.1 )
faltan

for(i in 1:nrow(delitos.2) ){
  if(delitos.2$fecha.1[i] %in% faltan)
    delitos.2$fecha.1[i] = delitos.2$fecha.1[i]+1
}
delitos.2$TMedia <- NULL
delitos.3 <- left_join(delitos.2, temps.2)

faltan <- unique( filter(delitos.3, is.na(TMedia) )$fecha.1 )
faltan

for(i in 1:nrow(delitos.3) ){
  if(delitos.3$fecha.1[i] %in% faltan)
    delitos.3$fecha.1[i] = delitos.3$fecha.1[i]+2
}
delitos.3$TMedia <- NULL
delitos.4 <- left_join(delitos.3, temps.2)

faltan <- unique( filter(delitos.4, is.na(TMedia) )$fecha.1 )
faltan

library(Hmisc)
delitos.4$temp <- cut2(delitos.4$TMedia, g=5)

filter(delitos.4, fecha.1 %in% faltan)

temp <- delitos.4 %.% group_by(colonia, temp) %.%
  summarise( long= mean(long), lat=mean(lat),n=n()) %.% arrange( -n)

temp.1 <- delitos.4 %.% group_by(temp) %.%
  summarise( long= mean(long), lat=mean(lat),n=n()) %.% arrange( -n)

temp.1$prop <- round(100*temp.1$n/sum(temp.1$n),0)

x <- arrange(temp.1[,c('temp','n','prop')], temp)
names(x) <- c('Temperatura','Número','Proporción')

print(xtable(x, digits = 0  ), include.rownames=F)

ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Temp. media", size = "Número de delitos", alpha = "")  + 
  geom_point(data =temp  , aes(x = long, y = lat, size = n, colour=factor(temp)), alpha = .8) 
ggsave(filename = 'graphs/temperatura.pdf', width = 9, height = 6 )


delitos.4$dia.sem <- substr(format(delitos.4$fecha.1, "%a %b %d %H:%M:%S %Y"), start=1, stop=3)
delitos.4$dia.sem

dias <- delitos.4 %.% group_by(colonia,dia.sem)  %.% 
  summarise( long= mean(long), lat=mean(lat),n=n()) %.% arrange( -n)

write.csv(dias, file='data/dias.csv')


ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Temp. media", size = "Número de delitos", alpha = "")  + 
  geom_point(data =dias  , aes(x = long, y = lat, size = n, colour=dia.sem), alpha = .8) 

ggsave(filename = 'graphs/dias.pdf', width = 9, height = 6 )


ls()
head(Colonias)
x <- toupper(Colonias$NomColonia)
y <- toupper(delitos.4)



unique(x)
unique(y)
#cache('zapopan.fuerte')
#cache('delitos.4')

