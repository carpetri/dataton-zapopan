
# Mapeo de datos

##############  Clorophleth maps
library(MASS)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(plyr)
library(maptools)
library(maps)
library(SpatialEpi)
library(fields)

## Datos de variación continua
ozone <- read.table("~/Dropbox/MA/Biostats_696/Ozone_Aug1_2001.txt", header=T)
head(ozone)
plot(ozone$longitude, ozone$latitude, xlab="Longitud", ylab="Latitud",
  type="p", pch=20, col="black")
# el mapa de EUA se puede obtener del paquete fields
US(xlim=c(-130,-65),ylim=c(22,52), add=T,lwd=1,col="gray") # fields

# El mapa anterior únicamente indica la ubicación de las estaciones de observación
# mas no los valores observados.
# Vamos a dicretizar la variable ozono

plotvar <- ozone$ozone
nclr <- 9 # número de colores que usaremos
plotclr <- brewer.pal(nclr,"YlOrRd") # paleta

# Crear clases para la variable
class <- classIntervals(plotvar, nclr, style = "fixed", 
  fixedBreaks = seq(min(plotvar), max(plotvar), length = nclr + 1))
names(class)
# Asignamos un color a cada clase
colcode <- findColours(class, plotclr)

plot(ozone$longitude, ozone$latitude, xlab="Longitud", ylab="Latitud",
  type="p", pch=20, col = colcode, main = "Concentración de ozono (08/31/2001)")
US(xlim = c(-130,-65), ylim = c(22,52), add = T, lwd = 1,col = "gray") # fields

# Agregar las leyendas de color: image.plot
length.x <- 25
length.y <- 25

x.grid <- seq(min(ozone$longitude), max(ozone$longitude), length = length.x)
y.grid <- seq(min(ozone$latitude), max(ozone$latitude), length = length.y)
z.grid <- matrix(NA, nrow = length.x, ncol = length.y)
z.lim <- range(plotvar)

image.plot(x.grid, y.grid, z.grid, xlab = "Latitude", ylab = "Longitude", 
  zlim = z.lim, col = plotclr, breaks = class$brks, legend.lab = "ppb", 
  main = "Concentración de ozono (08/31/2001)")
US(xlim=c(-130,-65), ylim = c(22,52),add=T,lwd=1,col="gray")
points(lon,lat,col=colcode,pch=20)

## hacerlo en ggplot!
# Algunos mapas están en la librería maps de R
map("world")
map("usa")
map("state")
map("county")
US <- map("state", fill=TRUE, plot=FALSE) # maps
names(US)
## obtenemos los ids
US.names <- US$names
US.IDs <- sapply(strsplit(US.names,":"),function(x) x[1])
## transformamos a clase polígono espacial 
## Mas de proyecciones: http://trac.osgeo.org/proj/wiki/GenParms#Parameterlist
US.poly <- map2SpatialPolygons(US, IDs = US.IDs, 
  proj4string = CRS("+proj=longlat +datum=wgs84")) # SpatialEpi
class(US.poly)

shape.fort <- fortify(US.poly) # ggplot
shape.fort <- shape.fort[order(shape.fort$order), ] 
head(shape.fort)
ozone$o3 <- cut(ozone$ozone, breaks = class$brks, include.lowest = T)
ggplot(data = shape.fort, aes(long, lat)) + 
  geom_polygon(colour='gray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 5/3.5) +
  labs(title = "Concentración de ozono (08/31/2001)", x = "", y = "", 
    colour = "ppb") + 
  geom_point(data = ozone, aes(x = longitude, y = latitude, colour = o3)) +
  scale_colour_brewer(palette="YlOrRd") 


### Datos de variación dicreta
eire <- readShapePoly(system.file("etc/shapes/eire.shp", package = "spdep")[1],
  ID = "names", proj4string = CRS("+proj=utm +zone=30 +units=km"))
class(eire)

### Mapeamos el % de sangre tipo A en las muestras
plotvar <- eire$A
nclr <- 9

plotclr <- brewer.pal(nclr,"Purples")
class <- classIntervals(plotvar, nclr, style = "fixed", 
  fixedBreaks = seq(min(plotvar), max(plotvar), length = nclr + 1))
class

### To see which are the class breaks (we will need this to make the legend)
colcode <- findColours(class,plotclr)
plot(eire,border="black",axes=TRUE,xlim=c(-200,400))
title(xlab="Eastings (km)",ylab="Northings (km)",main="Percentage of blood A samples")
plot(eire,col=colcode,add=T)

leg.txt<-c("[23.9%,25.2%)","[25.2%,26.6%)","[26.6%,27.9%)","[27.9%,29.2%)",
"[29.2%,30.6%)","[30.6%,31.9%)","[31.9%,33.2%)","[33.2%,34.5%)","[34.5%,35.9%]")
legend(-200,6100,legend=leg.txt,fill=plotclr,cex=1,ncol=1,bty="n")

### Usando ggplot 
shape.fort <- fortify(eire, group = "names") 
shape.fort <- shape.fort[order(shape.fort$order), ] 
head(shape.fort)

# El atributo que vamos a representar es % Sangre tipo A
sub.eire <- data.frame(id = eire$names, A.blood = eire$A)
head(sub.eire)

eire.map <- join(shape.fort, sub.eire, by = "id")
cuts <- seq(min(eire.map$A.blood), max(eire.map$A.blood, na.rm = T), 
  length = 10)
eire.map$A.class <- cut(eire.map$A.blood, cuts, include.lowest = TRUE)
head(eire.map)
ggplot(data = eire.map, aes(long, lat)) + 
  geom_polygon(aes(fill = A.class, group = group))  +
  geom_path(color="darkgray", size = 0.15, aes(group = group)) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Muestras de sangre en Iralnda", x = "Eastings",
    y = "Northings", fill = "% tipo A") + 
  theme(legend.title.align = 0.5) +
  coord_fixed()

# Mexico
# http://blog.diegovalle.net/2013/06/shapefiles-of-mexico-agebs-manzanas-etc.html
load("~/Dropbox/curso_ITAM/homicidios/map_mx.RData")

class(mexico.shp)
shape.fort <- fortify(mexico.shp) 
shape.fort <- shape.fort[order(shape.fort$order), ] 

# Tasa de emigración a EUA (por cada 10 mil habitantes)
migracion <- read.csv("~/Dropbox/curso_ITAM/homicidios/migracion_EUA.csv")[1:32, ]
migra <- data.frame(id = 0:31, estado = mexico.shp$NAME_1, 
  tasa = migracion$X2009)
map.mexico <- join(shape.fort, migra, by = "id")
head(map.mexico)
cuts <- seq(min(map.mexico$tasa), max(map.mexico$tasa, na.rm = T), length = 7)
map.mexico$tasa.class <- cut(map.mexico$tasa, cuts, include.lowest = TRUE)
head(map.mexico)
ggplot(data = map.mexico, aes(long, lat, group=group)) + 
  geom_polygon(aes(fill = tasa.class, group = group))  +
  geom_path(color="gray", size = 0.15, aes(group = group)) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Tasa de emigración a EUA", x = "",
    y = "", fill = "Tasa") + 
  theme(legend.title.align = 0.5) + coord_fixed() # + 
  # geom_text(data = nom, aes(x = long, y = lat, label = estado, group = group))

nom <- ddply(map.mexico, "estado", summarise, long = mean(long), lat = mean(lat), 
  group = group[1])


# Municipios
# aea: Albers equal area
mex <- readShapePoly("~/Dropbox/curso_ITAM/homicidios/maps/MUNICIPIOS.shp", 
  verbose = TRUE, proj4string = CRS("+proj=aea +units=km"))

mex.mun <- subset(mex, CVE_ENT == "11")

shape.fort <- fortify(mex.mun) 
shape.fort <- shape.fort[order(shape.fort$order), ] 
ggplot(data = shape.fort, aes(long, lat, group=group)) + 
  geom_polygon(colour='black',
    fill='white') + coord_fixed() +
  labs(title = "Guanajuato", x = "", y = "") 