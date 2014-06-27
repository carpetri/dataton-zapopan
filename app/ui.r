
shinyUI(
  
  pageWithSidebar(
    
    # Application title
    headerPanel(
      list(HTML('<img src="icon.jpg" style="width: 90%; height: 20%" />'), 
           "Delitos en Zapopan - PETRUS"
      ),
      windowTitle="PETRUS"  ),
    
    
    sidebarPanel(
      
      h3("Selecciona la colonia"),
      
      selectInput(inputId="colonia",label="Nombre de la colonia",
                  list("Uno" = "Peatonal", 
                       "Mixta" = "Mixta", 
                       "Vehicular" = "Vehicular"), selected='Vehicular')
      ,
      

      
      selectInput("X24hrs", "Abre 24 horas",
                  list("No" = 'no',"Si" = 'si'), selected='Si')
      
      
    )#slider
    ,
    
    mainPanel(
      
      
      h3('Predicción'),
      h2(as.character(verbatimTextOutput("data"))),
      numericInput("corte", 
                   "Corte de ventas diarias por plaza operativa", 
                   min = 20,
                   max = 1000, 
                   value = 53400),
      plotOutput("plot2", width = "100%", height = "120px"),
      h3('Rangos para variables'),
      plotOutput("plot", width = "100%", height = "300px")
      ,
      br(),
      br(),
      br(),
      
      p("Creado por:",
        a("Carlos Petricioli", href = "http://github.com/carpetri"), 'y', 
        a("Christopher Lazaris", href = "http://clazarus.com") )
      # downloadButton('reporte','Reporte')
      
      
      
    )))