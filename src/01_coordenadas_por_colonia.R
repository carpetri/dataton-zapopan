library(ProjectTemplate)
reload.project()


delitos.1 <- filter( delitos, x >0)
delitos.detenidos.1 <- filter(delitos.detenidos, x>0 )

delitos.detenidos.1$tipo <- gsub(delitos.detenidos.1$tipo, pattern = 'FALTAS ADMINISTRATIVAS', 
                                 replacement = 'FALTA ADMINISTRATIVA')


coord.colonias <- delitos.1 %.% group_by(colonia) %.% summarise( long= mean(long), lat=mean(lat),
                                                                 n=n())


write.csv(coord.colonias,'data/coordenas_por_colonia.csv')


delitos.1$fecha.1 <- as.Date(delitos.1$fecha, format="%d/%m/%Y")

delitos.1$fecha.1
delitos.1$mes <- month(delitos.1$fecha.1)

table(delitos.1$mes)
