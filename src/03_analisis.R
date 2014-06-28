library(ProjectTemplate)
reload.project()


delitos.detenidos$colonia <- toupper(delitos.detenidos$colonia)
coordenas.por.colonia$colonia <- toupper(coordenas.por.colonia$colonia)

coord.2 <- filter(delitos.detenidos,x>0) %.% group_by(colonia) %.% summarise(
  n=n(), long=mean(long), lat=mean(lat)
  )

length(unique(delitos.4$colonia))

head(coordenas.por.colonia)
dat <- left_join(coordenas.por.colonia[,c('colonia','n')], coord.2[,c('colonia','n')], by='colonia')
nrow(dat)

names(dat) <- c( "colonia",   "n.cometidos"     ,"n.detenidos"  )

head(arrange(dat, -n.cometidos      ))

dat$div  <- dat$n.cometidos / dat$n.detenidos

dat <- na.omit(dat)
dat <- arrange(dat, div)
filter(dat, n.cometidos>10 )

dat$color <- TRUE

dat[ dat$n.cometidos>55&  dat$div >1 ,'color'] <-   FALSE
  


ggplot(dat,aes(x=n.cometidos, y=n.detenidos, colour=!color), alpha=1)+ geom_point() + 
  geom_abline(intercept=0,slope=1,col='black', alpha=.5) + xlim(0,400)+ylim(0,400)+
  scale_color_manual(values=c('blue','Red')) + xlab('Número de delitos cometidos') +
  ylab('Número de delitos detenidos') +ggtitle('Colonias en Zapopan')+
  labs(color='Colonias por observar') 
  
ggsave('graphs/recomendacion.pdf',width = 9, height = 6 )

x <- dat[ dat$n.cometidos>55&  dat$div >1 ,c('colonia','n.cometidos' ,
                                             'n.detenidos')]
names(x) <- c('Colonia','Cometidos' , 'Detenidos')
library(xtable)
print(xtable(x,caption='Colonias a observar' ), include.rownames=F )

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



