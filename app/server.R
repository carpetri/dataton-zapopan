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
        scale_color_manual(values=rev(brewer.pal(5,"RdBu") ))  #+
        #geom_point(data =coord.colonias  , aes(x = long, y = lat,size=n ), alpha = .7, color='black') 
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
  
  output$plot2  <-  renderPlot({
    
    inter.1 <- subset(inter(), variable %in% c('prom.ventas') ) 
    prom.ventas <- exp( mean(sapply(1:3, function(i) {predict(mod.boost.lista[[i]],newdata=dato(),se.fit=T)[1]}))
    ) /30.5
    up <-  prom.ventas*(1+.28) 
    lb <-  prom.ventas*(1-.28) 
    
    print(plot.2 <- qplot(x='Promedio Ventas',y=prom.ventas,ymin=lb,ymax=up ,
                          label=as.character(round(prom.ventas))  ) + 
            ylim(min(base.2$prom.ventas/30),max(base.2$prom.ventas)/30 )+
            xlab('')+ylab('')+
            geom_linerange(size=2,color='grey')+
            geom_hline(yintercept=input$corte, size=2,alpha=.8)+
            geom_point(size=10,color='grey', alpha=.7 ) +    geom_text(colour='black') +
            
            theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20), legend.position='none' )+ coord_flip() 
    )
    #save(plot.2, file='plot_2.Rd')
    
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
