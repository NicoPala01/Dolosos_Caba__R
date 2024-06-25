
#TP FINAL ALGORITMOS Y ESTRUCTURAS DE DATOS
#NICOLAS PALAVECINO

rm(list = ls())
options(scipen=999)

# Obtener directorio del script actual y modificar el working directory
library("rstudioapi")  
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
library(tidyverse)
library(dplyr)


#Leer datos
snic <- read_delim("../datos/snic-departamentos-mes-sexo.csv")
poblacion <- read_delim("../datos/caba_poblacion_comunas.csv", delim = ";")


#Verifico clase de campos
class(snic$cantidad_victimas)
class(snic$cantidad_victimas_masc)
class(snic$cantidad_victimas_fem)
class(snic$departamento_id)
class(snic$provincia_id)
class(snic$codigo_delito_snic_id)


#Convierto a numericos los campos solicitados
snic$departamento_id <- as.numeric(as.character(snic$departamento_id))
snic$provincia_id <- as.numeric(as.character(snic$provincia_id))
snic$codigo_delito_snic_id <- as.numeric(as.character(snic$codigo_delito_snic_id))


#Convierto a cero los NA

snic$cantidad_victimas_masc[is.na(snic$cantidad_victimas_masc)] <- 0
snic$cantidad_victimas_fem[is.na(snic$cantidad_victimas_fem)] <- 0 
snic$cantidad_victimas[is.na(snic$cantidad_victimas)] <- 0
snic$cantidad_victimas_sd[is.na(snic$cantidad_victimas_sd)] <- 0 
snic$codigo_delito_snic_id[is.na(snic$codigo_delito_snic_id)] <- 0 
colSums(is.na(snic))


#Filtrar datos de delitos
snic <- filter(snic, anio>=2014 & anio<=2022)
snic <- filter(snic, provincia_id==2)
snic <- filter(snic, codigo_delito_snic_id == 1)

#Agrupar homicidios dolosos por año
snic_dolosos_año <- snic %>% group_by(anio) %>% summarize(
            cantidad_victimas_masc = sum(cantidad_victimas_masc),
            cantidad_victimas_fem = sum(cantidad_victimas_fem),
            cantidad_victimas = sum(cantidad_victimas)
)


#Agrupar por año la poblacion y filtrar 2014 a 2022
poblacion_anio <- poblacion%>% group_by(anio) %>% summarize(poblacion_total = sum(total))
poblacion_anio <- filter(poblacion_anio, anio>=2014 & anio<=2022)


#Nuevo dataset combinando delitos y poblacion
dolosos_agrupados <- merge(snic_dolosos_año, poblacion_anio, by= "anio")


#Agrego columnas nuevas
dolosos_agrupados <- dolosos_agrupados%>%
  mutate(
    porcentaje_masc = ifelse(cantidad_victimas!=0, cantidad_victimas_masc * 100 / cantidad_victimas,0),
    porcentaje_fem = ifelse(cantidad_victimas!=0,cantidad_victimas_fem * 100 / cantidad_victimas, 0),
    tasa_por_100k = ifelse(cantidad_victimas!=0,cantidad_victimas / (poblacion_total/100000), 0)
  )


#Quito decimales
dolosos_agrupados$porcentaje_masc <- dolosos_agrupados$porcentaje_masc %>% round(digits = 1)
dolosos_agrupados$porcentaje_fem <- dolosos_agrupados$porcentaje_fem %>% round(digits = 1)
dolosos_agrupados$tasa_por_100k <- dolosos_agrupados$tasa_por_100k %>% round(digits = 1)

#Cambio orden de columnas

dolosos_agrupados <- dolosos_agrupados %>%
  select(anio, cantidad_victimas_masc, cantidad_victimas_fem, cantidad_victimas, porcentaje_masc, porcentaje_fem, tasa_por_100k, poblacion_total)


#Genero CSV
write.csv(dolosos_agrupados, file = "homicidios_dolosos_agrupados.csv", row.names = FALSE)


#EJERCICIO 1-B)
#install.packages("plotly")
library(plotly)
tasa_maxima <- max(dolosos_agrupados$tasa_por_100k)
fig <- plot_ly(dolosos_agrupados)%>%
      add_bars(x= ~anio, y = ~cantidad_victimas, 
               name = 'Victimas', yaxis = 'y1', 
               marker = list(color = "lightgreen"), 
               width = 0.5, text = ~cantidad_victimas,
               textposition = 'auto', insidetextanchor = 'middle') %>%
      add_trace(x = ~anio, y= ~tasa_por_100k,
                name = 'Tasa', 
                yaxis = 'y2',
                type = 'scatter',
                mode = 'markers+text', 
                line = list(color = 'red', size = 8, symbol = 'circle', shape = 'spline'),
                text = ~round(tasa_por_100k, 2), 
                textposition = 'top') %>%
      layout(
  title = "Victimas Totales y  tasa por 100.000",
  yaxis = list(title = "Victimas Totales"),
  yaxis2 = list(title = "Victimas por 100.000", overlaying = 'y', side = 'right', range = c(0, tasa_maxima), dtick = 1),
  xaxis = list(title = "Año"),
  legend = list(x = 0.8, y = 0.9)
)

fig

#EJERCICIO 1-C)




#Selecciono variables

dolosos_por_sexo <- dolosos_agrupados
dolosos_por_sexo$tasa_por_100k <- NULL
dolosos_por_sexo$poblacion_total <- NULL

#Ordeno variables
dolosos_por_sexo <- dolosos_por_sexo %>%
  select(anio, cantidad_victimas_masc, porcentaje_masc, cantidad_victimas_fem, porcentaje_fem, cantidad_victimas)
write.table(dolosos_por_sexo, file = "Víctimas de homicidios dolosos por sexo. Valores absolutos y participación")



#EJERCICIO 2

#Utilizo dataset del ejercicio 1
#Filtrar para 2022

snic_departamentos_mes_sexo <- filter(snic, anio==2022)
poblacion_caba_2022 <- filter(poblacion,anio==2022)