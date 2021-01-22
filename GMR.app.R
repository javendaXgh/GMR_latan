library(dplyr)
#library(tidyverse)
#library(RColorBrewer)
#library(lubridate)
#library(gridExtra)
library(scales)
#library(plyr)
#library(ggrepel)
#library(viridis)
library(wesanderson)
#library(ggthemes)
library(shiny)
library(shinyWidgets)
library(ggplot2)

#download.file('https://github.com/javendaXgh/GMR_latan/raw/master/global.rds','global.rds')

global <- readRDS('global.rds')

dias_laborales <- c('viernes','lunes','jueves','martes','miercoles')

paises <- unique(global$region)

listado_paises <- paises

#variables leyenda grafico
caption=paste("datos obtenidos de www.google.com/covid19/mobility/ procesados @javendaXtw. Actualizados al",max(global$fecha))


global_filtros <- function (paises, 
                            sub_nac=FALSE,
                            post=FALSE,
                            weekend=FALSE,
                            inicio=FALSE,
                            fin=FALSE){
  #fechas por defecto
  if(inicio!=FALSE & fin==FALSE){fin=max(global$fecha)}
  if(fin!=FALSE & inicio==FALSE){inicio=min(global$fecha)}
  
  filter(global,region %in% paises)->global
  global$region <- as.character(global$region)
  
  
  # if(sub_nac!=FALSE){
  #   filter(global, sub_reg1 %in% sub_nac) ->global
  # }else{
  #   filter (global, sub_reg1=='') -> global
  # }
  if(post==TRUE){
    filter(global, fecha>=inicio_ven) -> global
  }
  if(weekend==TRUE){
    filter(global, dia_semana %in% dias_laborales)-> global
  }
  if (inicio!= FALSE){
    filter(global, fecha>=inicio&fecha<=fin)-> global
  }
  #print(global)
  return(global)
}

#funcion listado sub_regiones_1
levels_sub_reg1 <- function(region_){
  global%>%
    filter(region==region_) -> seleccion
  #print(unique(seleccion$sub_reg1))
}

#listado de datos a graficar
tipo_elemento <- names(global)[6:11]


gg_comp_paises <- function(paises, 
                           sub_nac=FALSE,
                           post=FALSE,
                           weekend=FALSE,
                           inicio=FALSE,
                           fin=FALSE,
                           elemento,
                           hitos=FALSE){
  #llamada a funcion para generar df
  df <- global_filtros(paises = paises,
                       sub_nac = sub_nac,
                       post=post,
                       weekend = weekend,
                       inicio= inicio,
                       fin=fin)

  #variables de ayuda para grafico
  fecha_f=max(global$fecha)
  fecha_i=min(global$fecha)
  posicion_leyenda=2
  if(inicio!=FALSE){fecha_i=as.Date(inicio)}
  if(fin!=FALSE){fecha_f=as.Date(fin)}
  if(mean(df[,elemento], na.rm = TRUE)<0){posicion_leyenda=-10}
  
  nombre_paises=paste(paste(paises,collapse = '-'),
                      if(sub_nac){paste(sub_nac,collapse = '-')})
  ggplot(df, aes(x=fecha,colour=region) ) +
    aes_string(y=elemento)+
    geom_line(size=1.4)+
    #geom_point()+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    theme_bw()+
    #theme(panel.background = element_rect( colour = "#6D9EC1",#fill = "snow",
    #size = 2, linetype = "solid"))+
    theme(legend.position = 'bottom')+
    labs(subtitle = paste('Sector: ',gsub('_',' ',elemento),". desde: ",format(min(df$fecha),'%d-%m-%Y'),
                          ' hasta:',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    
    #scale_color_viridis(discrete = TRUE, option = "D")+
    #scale_fill_viridis(discrete = TRUE) +
    #scale_x_date(date_breaks='1 week',labels = date_format('%d-%m-%Y'))+
    scale_x_date(date_breaks='4 week',labels = date_format('%d-%m-%Y'))+
    ggtitle(paste('Cambios en movilidad: ',nombre_paises))+
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    
    theme(panel.background = element_rect(fill = 'grey70', colour = 'red'))+
    scale_color_manual(values = wes_palette("Royal1", n = length(paises)))+
    #grids(axis = c("xy", "x", "y"), color = "grey50",linetype = "dashed")+#BottleRocket1,Darjeeling2,Royal1
    #scale_color_brewer(palette = "Set1")+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="red",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
}  

df_eventos <- data.frame(
  a=as.Date(c('2020-04-09','2020-04-12','2020-05-01')),
  b=c('inicio Semana Santa','fin de Semana Santa','Día del trabajador'),
  stringsAsFactors = F)



#source('funciones.R')




# gg_comp_paises(paises=c('Venezuela', 'Colombia'),
#                sub_nac=FALSE,
#                post=FALSE, 
#                weekend=FALSE, 
#                inicio='2020-09-01',
#                #fin='2020-05-14',
#                elemento='zonas_residenciales',
#                hitos=TRUE)
sector <- c('lugares de trabajo'='lugares_de_trabajo',
            'supermercados y farmacias'='supermercados_y_farmacias',
            'parques'='parques',
            'zonas residenciales'='zonas_residenciales',
            'estaciones de transporte publico'='estaciones_de_transporte_publico',
            'tiendas y ocio'='tiendas_y_ocio')

# Define UI for dataset viewer app ----
ui <- shinyUI(fluidPage(
  setBackgroundColor(
    color = "#003049"), #2A9D8F,264653
 
  # App title ----
  #titlePanel("Gráficos con datos del
             #Google Mobility Report para países de Sur América"),
  h1(id="big-heading", "Gráficos con datos del
             Google Mobility Report para países de Sur América"),
  tags$style(HTML("#big-heading{color: #e9c46a;}")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("pais", "Elegir un país:",
                  choices=paises ,
                  selected = 'Venezuela',
                  multiple=TRUE),
      selectInput('sector', 'Elegir un sector:',
                  choices=names(sector)),
      dateRangeInput('dateRange',
                     label = 'Seleccionar rango de fechas: (año-mes-día) ',
                     start = min(global$fecha), end = max(global$fecha)
      ),
      
      awesomeCheckbox('we_ends','Excluir fines de semana:',
                      value = FALSE,status = "info"),
      br(),

      actionButton("update", "generar gráfico"),
      helpText("Instrucciones de uso:"),
      helpText("1) Seleccionar uno o más países colocando el puntero en el campo 'elegir un país' e ir añadiendo uno a uno "),
      helpText("2) Seleccionar el sector a visualizar en el campo 'Elegir un sector"),
      helpText("3) Seleccionar un rango de fechas no anterior al 2020-01-15 (formato 'año-mes-día')"),
      helpText("4) Seleccionar, o deseleccionar, botón para 'excluir fines de semana' del gráfico"),
      helpText("5) Generar o actualizar el gráfico presionando el botón 'generar gráfico"),
      helpText("6) Para eliminar un país seleccionarlo en el campo 'Elegir un país' con click en el mouse y presionar delete"),
      helpText("+ info www.google.com/covid19/mobility"),
      br(),
      helpText('optimizado para visualizarse en pantallas PC o laptops.'),
      helpText("en teléfonos celulares no se aprecia toda la información"),
      
      width = 3 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("plot",width = "100%")
    )
    
  )
))

# Define server logic to summarize and view selected dataset ----
server <- shinyServer(function(input, output) {
  
  
  h <- eventReactive(input$update,{
    elemento = switch(input$sector,
                      'lugares de trabajo'='lugares_de_trabajo',
                      'supermercados y farmacias'='supermercados_y_farmacias',
                      'parques'='parques',
                      'zonas residenciales'='zonas_residenciales',
                      'estaciones de transporte publico'='estaciones_de_transporte_publico',
                      'tiendas y ocio'='tiendas_y_ocio')
    
  })
  grafico <- eventReactive(input$update,{
    gg_comp_paises(paises=input$pais,
                   inicio=input$dateRange[1],
                   fin= input$dateRange[2],
                   elemento=h(),
                   weekend=input$we_ends
    )
  })
  
  
  output$plot <- renderPlot({
    grafico()
    
  },height = 620 )
  
})

# Create Shiny app ----
shinyApp(ui=ui,server= server)


