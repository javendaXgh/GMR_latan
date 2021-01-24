library(gganimate)
global <- readRDS('global.rds')

inicio_ven <- as.Date('2020-03-17')
esp <- c('viernes','lunes','sabado','domingo','jueves','martes','miercoles')
meses <- c('abril','febrero','marzo','mayo', 'junio')
sur_america <- c('Argentina','Bolivia', 'Brazil','Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela')
col_names0 <- c('cod_pais','region',
                'sub_reg1','sub_reg2',
                'fecha',
                'tiendas_y_ocio',
                'supermercados_y_farmacias',
                'parques',
                'estaciones_de_transporte_publico',
                'lugares_de_trabajo',
                'zonas_residenciales')

col_names1 <- c('cod_pais','region',
                'sub_reg1','sub_reg2',
                'fecha',
                'tiendas_y_ocio',
                'supermercados_y_farmacias',
                'parques',
                'estaciones_de_transporte_publico',
                'lugares_de_trabajo',
                'zonas_residenciales')

dias_laborales <- c('viernes','lunes','jueves','martes','miercoles')






#cargar datos y transformarlos






listado_paises <- levels(global$region)

#variables leyenda grafico
caption=paste("datos obtenidos de www.google.com/covid19/mobility/ procesados @javendaXtw. Actualizados al",
              max(global$fecha))

#funcion de ayuda generacion graficos
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
  
  
  if(sub_nac!=FALSE){
    filter(global, sub_reg1 %in% sub_nac) ->global
  }else{
    filter (global, sub_reg1=='') -> global
  }
  if(post==TRUE){
    filter(global, fecha>=inicio_ven) -> global
  }
  if(weekend==TRUE){
    filter(global, dia_semana %in% dias_laborales)-> global
  }
  if (inicio!= FALSE){
    filter(global, fecha>=inicio&fecha<=fin)-> global
  }
  return(global)
}

#funcion listado sub_regiones_1
levels_sub_reg1 <- function(region_){
  global%>%
    filter(region==region_) -> seleccion
  print(unique(seleccion$sub_reg1))
}

#listado de datos a graficar
tipo_elemento <- names(global)[6:11]


#parametros generacion de graficos
#paises = vector con paises a seleccionar. listado en 'listado paises'
# sub_nac=FALSE vector con niveles sub_nacionales. Consultar listado con 'levels_sub_reg1('nombre_pais)'
# post=FALSE filtrar solo fechas posteriores a inicio_ven
# weekend=FALSE excluir fechas de fines de semana
# inicio=FALSE seleccion intervalo fecha inicio formato ('Y-m-d')
# fin=FALSE)seleccion intervalo fecha fin formato ('Y-m-d')

#funcion ayuda dos filtro
global_filtros_dos <- function(paises, 
                               sub_nac=FALSE,
                               post=FALSE,
                               weekend=FALSE,
                               inicio=FALSE,
                               fin=FALSE){
  #fechas por defecto
  if(inicio!=FALSE & fin==FALSE){fin=max(global$fecha)}
  if(fin!=FALSE & inicio==FALSE){inicio=min(global$fecha)}
  
  
  filter(global,region %in% paises )->global
  global$region <- as.character(global$region)
  global_tmp <- global
  global_tmp <- filter(global_tmp,sub_reg1=="")
  
  if(sub_nac!=FALSE){
    filter(global, sub_reg1 %in% sub_nac) ->global
  }else{
    filter (global, sub_reg1=='') -> global
  }
  global <- rbind(global_tmp,global)
  if(post==TRUE){
    filter(global, fecha>=inicio_ven) -> global
  }
  if(weekend==TRUE){
    filter(global, dia_semana %in% dias_laborales)-> global
  }
  if (inicio!= FALSE){
    filter(global, fecha>=inicio&fecha<=fin)-> global
  }
  
  return(global)
}

#funcion filtrar pais
df_pais <- function(pais){
  global%>%
    filter(region==pais)-> temp
  return(temp)
}

########data frame con eventos a destacar
df_eventos <- data.frame(
  a=as.Date(c('2020-04-09','2020-04-12','2020-05-01')),
  b=c('inicio Semana Santa','fin de Semana Santa','Día del trabajador'),
  stringsAsFactors = F)

gg_animado_paises <- lineas <- function(paises, 
                                        sub_nac=FALSE,
                                        post=FALSE,
                                        weekend=FALSE,
                                        inicio=FALSE,
                                        fin=FALSE,
                                        elemento,
                                        height_v = 400, 
                                        width_v =800,
                                        hitos=FALSE){
  
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
  
  animado <- ggplot(df, aes(x=fecha,,colour=region) ) +
    aes_string(y=elemento)+
    geom_hline(aes( yintercept=0), color="black", size=.3, alpha=.8)+
    geom_line(size=.8)+
    geom_point()+
    scale_colour_brewer(palette = 'Set1')+
    #facet_grid(region~.)+
    theme_linedraw()+
    #scale_fill_brewer('set1')+
    transition_reveal(fecha)+
    #labs( title= "En casa / fecha: {frame_along}")+
    #theme(panel.background = element_rect(fill = "snow", colour = "#6D9EC1",
    #size = 2, linetype = "solid"))+
    theme(panel.background = element_rect(fill = 'grey75', colour = 'red'))+
    labs(subtitle = paste('Sector: ',gsub('_',' ',elemento),". desde: ",format(min(df$fecha),'%d-%m-%Y'),
                          ' hasta:',format(max(df$fecha),'%d-%m-%Y')) ,
         caption = caption,
         #tag = "Gráfico 1",
         x = "fecha",
         y = "%",
         colour = "País")+
    #scale_x_date(date_breaks='1 week',labels = date_format('%d-%m-%Y'))+    
    scale_x_date(date_breaks='4 week',labels = date_format('%m-%Y'))+
    #scale_color_viridis(discrete = TRUE, option = "D")+
    #ggtitle(paste('Cambios en movilidad: ',nombre_paises))+
    #scale_color_manual(values = wes_palette("Royal1", n = length(paises)))+#BottleRocket1,Darjeeling2,Royal1
    scale_color_manual(values = '#023e8a')+#BottleRocket1,Darjeeling2,Royal1
    theme(plot.title=element_text(size=rel(1.5),
                                  lineheight=.9,family="Times",
                                  face="bold.italic",colour="grey38"))+
    #theme(legend.position = 'bottom')+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_vline(data=df_eventos, mapping=aes(xintercept=a), color="orange",linetype="dotted", size=.5, alpha=.5)}+
    {if (hitos==TRUE & fecha_i<=df_eventos$a[1]& fecha_f>=df_eventos$a[length(df_eventos$a)])
      geom_text(data=df_eventos,aes(x=a, label=b , y = posicion_leyenda), colour="black", angle=90, size=1.8)}
  
  animate(animado, height = height_v, width =width_v,fps=5,end_pause = 15)
} #end_pause = 30
#renderer = gifski_renderer(loop = F)


animado <- gg_animado_paises(c('Venezuela'),
                  sub_nac=FALSE,
                  weekend=TRUE, 
                  post=TRUE, 
                  inicio='2020-03-15',
                  fin=FALSE,
                  elemento='zonas_residenciales',
                  height_v = 400,#tamano vertical
                  width_v = 800,#tamano horizontal
                  hitos=FALSE)
anim_save('www/venezuela.gif',animado)



