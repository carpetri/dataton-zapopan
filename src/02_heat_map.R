library(ProjectTemplate)
reload.project()

x <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_XI.csv", header=FALSE)
y <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_YI.csv", header=FALSE)
z <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_ZI.csv", header=FALSE)

df <- data.frame(x,y,z)
names(df) <- c('long','lat','n')
head(df)


library(Hmisc)
df <- filter(df,lat>=20.57)

df$cat <- cut2(df$n,g=5)
levels(df$cat) <- c('Muy baja','Baja','Media', 'Alta','Muy alta')



library(RColorBrewer)
nclr <- 5# número de colores que usaremos
plotclr <- brewer.pal(nclr,"YlOrRd") # paleta



ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
  geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
  scale_colour_brewer(palette="YlOrRd") #+geom_point(data =delitos.4  , aes(x = long, y = lat ), alpha = .1, size=3) 

ggsave(filename = 'graphs/heat.pdf', width = 9, height = 6 )


coord.colonias <- delitos.4 %.% group_by(colonia,tipo) %.% summarise( long= mean(long), lat=mean(lat),
                                                                      n=n())


ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
  scale_colour_brewer(palette="YlOrRd") +
geom_point(data =coord.colonias  , aes(x = long, y = lat,size=n ), alpha = .7, color='black') 
ggsave(filename = 'graphs/heat_y_delitos.pdf', width = 9, height = 6 )



####### OTROS 2 CASOS


x <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_XIv2.csv", header=FALSE)
y <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_YIv2.csv", header=FALSE)
z <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_ZIv2.csv", header=FALSE)

df <- data.frame(x,y,z)
names(df) <- c('long','lat','n')
head(df)


library(Hmisc)
df <- filter(df,lat>=20.57)

df$cat <- cut2(df$n,g=5)
levels(df$cat) <- c('Muy baja','Baja','Media', 'Alta','Muy alta')

ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
  geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
  scale_colour_brewer(palette="YlOrRd") +
  geom_point(data =coord.colonias  , aes(x = long, y = lat,size=n ), alpha = .7, color='black') 
ggsave(filename = 'graphs/heat_y_delitos2.pdf', width = 9, height = 6 )

delitos$colonia

write.csv(coordenas.por.colonia[,c('long','lat','colonia')],'x.csv' ) 

nrow(coordenas.por.colonia )
library(foreign)
coord.manz <- read.dbf(file = 'coord_delitos.dbf')

head(coord.manz)
nro
dat <- left_join(coordenas.por.colonia, coord.manz, by = 'colonia')
nrow(dat)
tail(dat)



dat.pob <- filter(dat , !is.na(CVEGEO) ) 
nrow(dat.pob)

mod <- lm(n~ POB1, data = dat.pob)
summary(mod)
plot(mod)

ggplot(dat.pob,aes(x=POB1, y=n))+ geom_point() +xlab('Población total') + 
  ylab('Número de delitos por colonia')
filter(dat.pob, POB1>1000)

nrow(x)

x <- dat.pob %.% group_by(n) %.% summarise( pob=sum(POB1) )



ggplot(filter(x,pob>0), aes(x=pob, y=n) ) +geom_point() 

nrow(x)

# ############# V3
# 
# 
# x <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_XIv3.csv", header=FALSE)
# y <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_YIv3.csv", header=FALSE)
# z <- read.csv("~/Desktop/dataton-zapopan/ipynb/heat_ZIv3.csv", header=FALSE)
# 
# df <- data.frame(x,y,z)
# names(df) <- c('long','lat','n')
# head(df)
# 
# 
# library(Hmisc)
# df <- filter(df,lat>=20.57)
# 
# df$cat <- cut2(df$n,g=5)
# levels(df$cat) <- c('Muy baja','Baja','Media', 'Alta','Muy alta')
# 
# ggplot(data = zapopan.fuerte, aes(long, lat)) + 
#   coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
#                                    colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
#   geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
#   geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
#   scale_colour_brewer(palette="YlOrRd") +
#   geom_point(data =coord.colonias  , aes(x = long, y = lat,size=n ), alpha = .7, color='black') 
#  



