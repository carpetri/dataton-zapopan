library(ProjectTemplate)
reload.project()

data.frame(cbind(unique(delitos.4$colonia),unique(delitos.detenidos$colonia) )


write.csv(sort(unique(delitos.4$colonia)), row.names = F, file='col.csv')

coord.2 <- filter(delitos.detenidos,x>0) %.% group_by(colonia) %.% summarise(
  n=n(), long=mean(long), lat=mean(lat)
  )

dat <- left_join(coordenas.por.colonia, coord.2, by='colonia')

nrow(dat)

names(dat) <- c("X"     ,  "colonia", "long"  ,  "lat"  ,   "n.cometidos"     ,"n.detenidos"  )

head(arrange(dat, -n.cometidos      ))

dat$div  <- dat$n.cometidos / dat$n.detenidos

xx <- head(arrange(dat[,c('colonia',"n.cometidos"     ,"n.detenidos",'div')], -div), 20)
xx
library(xtable)
print(xtable(xx,digits=0), include.rownames=F )

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

ggplot(data = zapopan.fuerte, aes(long, lat)) + 
  coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                   colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
  geom_point(data =df  , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
  geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
  #scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  +
  geom_point(data =coord.colonias  , aes(x = long, y = lat,size=n ), alpha = .7, color='black') 



