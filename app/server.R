library(shiny)
library(ggplot2)

load('../cache/delitos.4.Rdata')
load('../cache/heat.Rdata')
load('../cache/zapopan.fuerte.RData')

shinyServer(function(input, output) {
  
  
  output$plot <-renderPlot({
    
    
    if(input$cat!='Todos'){
    print(
      
    ggplot(data = zapopan.fuerte, aes(long, lat)) + 
        coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                         colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
        geom_point(data = heat[heat$cat==input$cat,] , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
        geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
      geom_point(data = delitos.4 , aes(x = long, y = lat ), alpha = .7, color='black') +
      scale_color_manual(values=rev(brewer.pal(5,"RdBu") )) 
    )   
    }else{
      ggplot(data = zapopan.fuerte, aes(long, lat)) + 
        coord_fixed(ratio = 7/10)+  labs(title = "Delitos en Zapopan", x = "Longitud", y = "Latitud", 
                                         colour = "Nivel de inseguridad", size = "Número de delitos", alpha = "")  + 
        geom_point(data = heat , aes(x = long, y = lat,  colour=cat ), alpha = .9, size=3) +
        geom_polygon(colour='darkgray', fill='white', aes(group=group), alpha=.5) +
        scale_color_manual(values=rev(brewer.pal(5,"RdBu") )) 
      
    }
    
  })
  
 
  output$reporte = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('input_reporte.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
  
})
