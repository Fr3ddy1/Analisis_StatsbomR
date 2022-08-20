#CARGO LIBRERIAS
library(StatsBombR)
library(lubridate)
library(dplyr)
library(purrr)
library(plotly)
library(png)
library(magick)


#OPCION 1: CARGAR EL WORKSPACE, ASI NO SE NECESITA TENER INSTALADO EL 
#PAQUETE StatsBombR
#load("data/data_goles.RData")

#OPCION2: TENER INSTALADO EL PAQUETE Y EJECUTAR LOS COMANDOS

################
################
################
#DATA DISPONIBLE
free_data <- FreeCompetitions() 

#COMPETICIONES DISPONIBLES
table(free_data$competition_name)

################
################
################
#SELECCIONO COMPETICION
#PARTIDOS DISPONIBLES DE LA EURO
free_data$competition_name[39]

Matches <- FreeMatches(free_data[39,])

################
################
################
#PARTIDOS EN ESPECIFICO
table(Matches$home_team.home_team_name)
table(Matches$away_team.away_team_name)

Matches1 <- Matches[which(Matches$home_team.home_team_name=="Italy" |
                            Matches$away_team.away_team_name=="Italy"),]


#HAGO CONSULTA DE LOS PARTIDOS DE LA EURO
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches1, Parallel = T) 

#REALIZO LIMPIEZA DE LAS COLUMNAS RELACIONADAS CON LAS COORDENADAS
events = allclean(StatsBombData) 
table(events$team.name)

#BASE DE DISPAROS SIN CONTABILIZAR TIROS DE PENALTY
shots = events %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Lorenzo Insigne") #1

#GRAFICO - LORENZO INSIGNE
insig <- ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = shots, aes(x = location.x, y = location.y, size = shot.statsbomb_xg
                               ,colour = factor(shot.outcome.name)))+
  scale_size(range = c(0, 10))+
  labs(title = "Lorenzo Insigne, Shot Map", subtitle = "EURO, 2021/22",
       size = "Statsbomb Xg", colour = "Shot outcome")+
  
  theme(
    plot.title = element_text(color="blue", size=18, face="bold.italic",hjust = 0.5),
    plot.subtitle = element_text(color="black", size=16, face="bold",hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_blank(),
    legend.direction = "horizontal",
    legend.position = "top",
    legend.margin = margin(c(20, 10, -85, 50)),
    legend.key.size = unit(1.5, "cm"),
    #panel.background = element_rect(fill = 'gray')
  ) +
  guides(size = guide_legend(title.position = "top"),
         colour = guide_legend(title.position = "top",override.aes = list(size = 6, fill = "black"))) +
  coord_flip(xlim = c(88, 125)) 

insig

#PROCESO PARA GUARDAR PLOT
#ESTABLEZCO ANCHO Y LARGO DE LA PANTALLA
screen_width <- 1920
screen_height <- 1080

#GUARDO
ggsave("images/Insig.png", insig, width = screen_width/130, height = screen_height/130,  dpi = 130)

#PROCESO PARA AGREGAR LOGO STATSBOMB
#LEO IMAGENES
plot <- image_read("images/Insig.png")

logo <- image_read("images/logo.png") %>%  image_resize(150)

#OBTENGO DIMENSIONES DEL PLOT
plot_height <- magick::image_info(plot)$height
plot_width <- magick::image_info(plot)$width

#OBTENGO DIMENSIONES DEL LOGO
logo_width <- magick::image_info(logo)$width
logo_height <- magick::image_info(logo)$height

#CALCULO COORDENADA Y PARA UBICAR LOGO
y <- plot_height - logo_height - plot_height * 0.01

#CALCULO COORDENADA X PARA UBICAR LOGO
x <- plot_width * 0.85

coord <- paste0("+",x,"+",y)

#GRAFICO MAS LOGO
new <- plot %>%
  image_composite(logo, offset = coord)

new

#GUARDO PLOT FINAL
image_write(new, path = "final_plots/Insig_logo.png", format = "png")

#FUNCION PARA GENERAR GRAFICO DE UN JUGADOR
#ARGUMENTOS:
#data: df a emplear, debe tener infromación sobre los disparos
#name: nombre del jugador a consultar
#max: valor maximo a mostrar luego del cambio de ejes
#titulo: string con el titulo del plot

goles_plot <- function(data,name,max = 125,titulo){
  #BASE DE DISPAROS
  shots = data %>%
    filter(type.name=="Shot" & player.name == name) #1
  
  #GRAFICO INICIAL
  a <- ggplot() +
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(),
          line = element_blank()) +
    # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6,
             x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+
    # add centre spot
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    geom_point(data = shots, aes(x = location.x, y = location.y, size = shot.statsbomb_xg
                                 ,colour = factor(shot.outcome.name)),alpha = 0.5)+
    scale_size(range = c(0, 10))+
    labs(title = paste0(name," , Shot Map"), subtitle = titulo,
         size = "Statsbomb Xg", colour = "Shot outcome")+
    
    theme(
      plot.title = element_text(color="blue", size=18, face="bold.italic",hjust = 0.5),
      plot.subtitle = element_text(color="black", size=16, face="bold",hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      legend.direction = "horizontal",
      legend.position = "top",
      legend.margin = margin(c(20, 10, -85, 50)),
      legend.key.size = unit(1.5, "cm")
    ) +
    guides(size = guide_legend(title.position = "top"),
           colour = guide_legend(title.position = "top",override.aes = list(size = 6, fill = "black"))) +
    coord_flip(xlim = c(range(shots$location.x)[1], max)) 
  
  #PROCESO PARA AGREGAR LOGO
  #GUARDO
  n <- paste0("images/",name,".png")
  ggsave(n, a, width = screen_width/130, height = screen_height/130,  dpi = 130)
  
  #PROCESO PARA AGREGAR LOGO STATSBOMB
  #LEO IMAGENES
  plot <- image_read(n)
  
  logo <- image_read("images/logo.png") %>%  image_resize(150)
  
  #OBTENGO DIMENSIONES DEL PLOT
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  #OBTENGO DIMENSIONES DEL LOGO
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  #CALCULO COORDENADA Y PARA UBICAR LOGO
  y <- plot_height - logo_height - plot_height * 0.01
  
  #CALCULO COORDENADA X PARA UBICAR LOGO
  x <- plot_width * 0.85
  
  coord <- paste0("+",x,"+",y)
  
  #GRAFICO MAS LOGO
  new <- plot %>%
    image_composite(logo, offset = coord)
  
  #GUARDO PLOT FINAL
  image_write(new, path = paste0("final_plots/",name,"_logo.png"), format = "png")
  
  #MENSAJE
  print("Gráfico generado y guarda en la ruta: ")
  print(paste0("final_plots/"))
  
  #IMPRIMO GRAFICO
  a
}#FINAL FUNCTION

#
#BUSCO GOLEADORES EURO
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T) 

events1 <- allclean(StatsBombData)

goles = events1 %>%
  filter(type.name=="Shot" & (shot.outcome.name == "Goal") ) #1

top <- (as.data.frame(table(goles$player.name)))
top <- top[order(top$Freq,decreasing = T),]
top1 <- top[c(1:6),] 
top1

#GRAFICOS
goles_plot(events1,name = "Cristiano Ronaldo dos Santos Aveiro",titulo = "EURO, 2021/22")
goles_plot(events1,name = "Harry Kane",titulo = "EURO, 2021/22")
goles_plot(events1,name = "Patrik Schick",max = 130,titulo = "EURO, 2021/22") 
goles_plot(events1,name = "Emil Peter Forsberg",titulo = "EURO, 2021/22")
goles_plot(events1,name = "Karim Benzema",titulo = "EURO, 2021/22")
goles_plot(events1,name = "Romelu Lukaku Menama",titulo = "EURO, 2021/22")




