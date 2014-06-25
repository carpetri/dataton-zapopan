library(ProjectTemplate)
reload.project()
library(gstat)
library(geoR)
ls()


#GEOESPACIAL
nclr <- 9
plotclr <- brewer.pal(nclr,"YlOrRd")
jalisco <- readShapeSpatial("data/inegi-jalisco/jal_ageb_urb.shp")
jalisco.fuerte <- fortify(jalisco)
head(jalisco.fuerte)

head(delitos)



ggplot(data = jalisco.fuerte, aes(long, lat)) + 
  geom_polygon(colour='darkgray', fill='white', aes(group=group)) + 
  coord_fixed(ratio = 5/3.5)+  labs(title = "AGEBS en Jalisco", x = "Longitud", y = "Latitud", 
       colour = "Residuales", size = "", alpha = "")  + 
   geom_point(data = delitos, aes(x = long, y = lat, colour = fecha),
              size = 3.5, alpha = 0.9) +   scale_colour_brewer(palette="YlOrRd") 




