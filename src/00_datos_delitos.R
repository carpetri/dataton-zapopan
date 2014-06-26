library(ProjectTemplate)
reload.project()
library(gstat)
library(geoR)
ls()


#GEOESPACIAL
nclr <- 9
plotclr <- brewer.pal(nclr,"YlOrRd")
agebs.jalisco <- readShapeSpatial("data/inegi-jalisco/jal_ageb_urb.shp")
agebs.jalisco.fuerte <- fortify(agebs.jalisco)
head(agebs.jalisco.fuerte)

mun.jalisco <- readShapeSpatial("data/inegi-jalisco/jal_municipal.shp")
mun.jalisco.fuerte <- fortify(mun.jalisco)

head(mun.jalisco.fuerte)


delitos.1 <- filter( delitos, x >0)



ggplot(data = jalisco.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 5/3.5)+  labs(title = "AGEBS en Jalisco", x = "Longitud", y = "Latitud", 
       colour = "Tipo de delito", size = "", alpha = "")  + 
   geom_point(data = delitos.1, aes(x = long, y = lat, colour = tipo),
              size = 1.5, alpha = 0.9) +   scale_colour_brewer(palette="YlOrRd") 



ggplot(data = mun.jalisco.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 5/3.5)+  labs(title = "Municipios en Jalisco", x = "Longitud", y = "Latitud", 
                                    colour = "Tipo de delito", size = "", alpha = "")  + 
  geom_point(data = delitos.1, aes(x = long, y = lat, colour = tipo),
             size = 1.5, alpha = 0.9) +   scale_colour_brewer(palette="YlOrRd") 
ggsave(filename = 'graphs/munucipios_y_delitos_jalisco.pdf', width = 9, height = 6 )





