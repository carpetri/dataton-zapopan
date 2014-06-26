library(ProjectTemplate)
reload.project()


delitos.1 <- filter( delitos, x >0)
delitos.detenidos.1 <- filter(delitos.detenidos, x>0 )

delitos.detenidos.1$tipo <- gsub(delitos.detenidos.1$tipo, pattern = 'FALTAS ADMINISTRATIVAS', 
                                 replacement = 'FALTA ADMINISTRATIVA')

