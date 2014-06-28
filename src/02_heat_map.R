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

x <- delitos.4 %.% group_by(colonia) %.% summarise(n=n())
arrange(x,n)

library(RColorBrewer)
nclr <- 5# número de colores que usaremos
plotclr <- brewer.pal(nclr,"RdBu") # paleta



ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
  geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
    scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  #+geom_point(data =delitos.4  , aes(x = long, y = lat ), alpha = .1, size=3) 

ggsave(filename = 'graphs/heat.pdf', width = 9, height = 6 )


coord.colonias <- delitos.4 %.% group_by(colonia,tipo) %.% summarise( long= mean(long), lat=mean(lat),
                                                                      n=n())


ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
    scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  +
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
heat <- df
cache('heat')

ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
  geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
    scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  +
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
#     scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  +
#   geom_point(data =coord.colonias  , aes(x = long, y = lat,size=n ), alpha = .7, color='black') 
#  




##########


data.frame(cbind(unique(delitos.4$colonia),unique(delitos.detenidos$colonia) )
           coordenas.por.colonia
           coord.2 <- filter(delitos.detenidos,x>0) %.% group_by(colonia) %.% summarise(
             n=n(), long=mean(long), lat=mean(lat)
           )
           
           dat <- left_join(coordenas.por.colonia, coord.2, by='colonia')
           
           nrow(dat)
           
           names(dat) <- c("X"     ,  "colonia", "long"  ,  "lat"  ,   "n.cometidos"     ,"n.detenidos"  )
           
           head(arrange(dat, -n.cometidos      ))
           
           dat$div  <- dat$n.cometidos / dat$n.detenidos
           
           arrange(dat, -div)
           
           
           dat
           
           head(coord.2)
           coord.2$tipo <- 'detenido'
           head(coordenas.por.colonia)
           coordenas.por.colonia$tipo <- 'cometido'
           head(coord.2)
           
           
           names(coord.2)
           names(coordenas.por.colonia)
           coordenas.por.colonia$X <- NULL
           x <- rbind(coord.2,coordenas.por.colonia)
           
           
           
           # ggplot(data = zapopan.fuerte, aes(long, lat)) + 
           #   geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
           #   
           #   coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
           #                                    colour = "Mes del delito", size = "Número de delitos", alpha = "")  + 
           #   geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
           #   #scale_colour_brewer(palette="RdBu") + 
           #   scale_color_manual(values=rev(brewer.pal(5,"RdBu") )) +
           #   geom_point(data =x , aes(x = long, y = lat, size = n), alpha = .8) 
           # + facet_wrap(~tipo)
           # 
           head(df)
           head(coord.colonias)
           
           ggplot(data = zapopan.fuerte, aes(long, lat)) + 
             coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                              colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
             geom_point(data =heat  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
             geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
             scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  +
             geom_point(data =x , aes(x = long, y = lat,size=n ), alpha = .7, color='black') +
             facet_grid(tipo~.)
           
           
           ggsave(filename = 'graphs/heat_facet.pdf', width = 9, height = 12 )
           
           




