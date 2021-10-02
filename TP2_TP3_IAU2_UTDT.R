#TP2

#A continuación descargaré las librerias necesarias para poder analizar los datos. 

library(rvest)
library(tidyverse)
library(stringr)
library(XML)


#Seguidamente procederé a importar los datos del Covid 19 en 2020 y 2021 a nivel mundial para empezar a analizarlos.

DatosCovid <- "https://es.wikipedia.org/wiki/Pandemia_de_COVID-19#Continentes_afectados"

Covid19 <- read_html(DatosCovid)

Covid19

#A continuación cargaré los datos del 2020.

Casos2020 <- Covid19 %>% 
  html_elements(".mw-parser-output > div:nth-child(47)") %>% 
  html_table()

Casos2020

#Ahora importaré los datos del 2021.

Casos2021 <- Covid19 %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[10]/table/tbody') %>% 
  html_table(fill=T)

Casos2021

#A continuación convertiré los datos obtenidos en dos tibble.

Casos2020.tabla<- Casos2020[[1]]

Casos2020.tabla


Casos2021.tabla <- Casos2021[[1]]

Casos2021.tabla

#En ambos casos las primeras filas no aportan datos significativos dado que busco centrarme en las muertes por covid de cada pais. 
#En este sentido, las eliminaré. 

Casos2020.tabla <-Casos2020.tabla %>% 
  slice(5:201)

Casos2020.tabla


Casos2021.tabla <-Casos2021.tabla %>% 
  slice(4:207)

Casos2021.tabla

#El propósito del trabajo será buscar si existe una relación de muertes por Covid entre los meses más fríos y más calurosos.
#Tomaré julio y agosto del 2020 y enero y febrero del 2021.
#Para eso, procederé a limpiar y a acomodar los datos.

Casos2020.tabla2 <- Casos2020.tabla [c(1,9:10)]

Casos2020.tabla2 <- Casos2020.tabla2 %>% 
  mutate(Jul = as.factor(Jul), Ago = as.factor(Ago))

Casos2020.tabla2<- Casos2020.tabla2 %>% 
  mutate(Jul = as.numeric(gsub(",", "", Jul)), Ago = as.numeric(gsub(",", "", Ago)))

names (Casos2020.tabla2) = c("Pais", "Julio2020", "Agosto2020")


Casos2021.tabla2 <- Casos2021.tabla [c(1,3:4)]

Casos2021.tabla2 <- Casos2021.tabla2 %>% 
  mutate(Ene = as.factor(Ene), Feb = as.factor(Feb))

Casos2021.tabla2<- Casos2021.tabla2 %>% 
  mutate(Ene = as.numeric(gsub(",", "", Ene)), Feb = as.numeric(gsub(",", "", Feb)))

names (Casos2021.tabla2) = c("Pais", "Enero2021", "Febrero2021")

#A continuación, comenzaré a unir las dos tablas para analizarlos con mayor profundidad. 

Casos.meses.calor.frio <- left_join(Casos2021.tabla2, Casos2020.tabla2, by="Pais")

Casos.meses.calor.frio <- mutate(Casos.meses.calor.frio, Total=Enero2021+Febrero2021+Julio2020+Agosto2020)

Casos.meses.calor.frio <- mutate(Casos.meses.calor.frio, TotalEyF2021=Enero2021+Febrero2021)

Casos.meses.calor.frio <- mutate(Casos.meses.calor.frio, TotalJyA2020=Julio2020+Agosto2020)

#Una vez ya unidos los datos comenzaré a calcular los porcentajes para corroborar la hipótesis: "En los meses más frios las víctimas aumentan". 

Porcentaje.Casos.meses.calor.frio <- Casos.meses.calor.frio %>% 
  mutate(Porcentaje.EyF.2021= TotalEyF2021 * 100 / Total) %>% 
  mutate(Porcentaje.JyA.2020 = TotalJyA2020 * 100 / Total)

Porcentaje.Casos.meses.calor.frio

#A modo de conclusión, es posible observar que no existe tal relación. 
#A traves del paso del tiempo, hubo más casos. 
#En otras palabras, aumentaron los casos de Coivd 19 a nivel mundial, sin importar la época del año.
#Esto podría estar relacionado a que con el paso del tiempo, las medidas de cuarentena extricta se fueron flexibilizando, las personas comenzaron a movilizarse y por ende aumentaron los casos. 

#TP3

#A continuación cargaré las librerias necesarias para profundizar el análisis previo y poder estudiar los datos obtenidos geograficamente. 

library(datos)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggrepel)
library(sf)
library(ggmap)
options(scipen=99)


#Para chequear la conclusión anterior procederé a graficar dichos datos con algunos paises de diferentes continentes.  


#Primero realizaré un filtro para reducir la muestra de paises. 

Porcentaje.Casos.meses.calor.frio.filtro <- filter (Porcentaje.Casos.meses.calor.frio, Pais %in% c("Estados Unidos", "Brasil", "MÃ©xico", "Argentina", "Reino Unido", "Francia", "Alemania", "EspaÃ±a", "Italia", "Dinamarca", "Rusia", "China", "India", "Emiratos Ãrabes Unidos", "Israel", "Australia", "SudÃ¡frica", "Marruecos", "Egipto"))

#Ahora grafiacaré los datos.

ggplot(Porcentaje.Casos.meses.calor.frio.filtro) + geom_point(aes(x=Pais, y=TotalJyA2020, color = "porcentaje cantidad de muertes"))+ labs(title="MUERTES POR COVID", subtitle = "Julio y Agosto 2020", x="Pais", y="Muertes", caption="Fuente: Wikipedia") 

ggplot(Porcentaje.Casos.meses.calor.frio.filtro) + geom_point(aes(x=Pais, y=TotalEyF2021, color = "porcentaje"))+ labs(title="MUERTES POR COVID", subtitle = "Enero y Febrero 2021", x="Pais", y="Muertes", caption="Fuente: Wikipedia")

ggplot(Porcentaje.Casos.meses.calor.frio.filtro) + geom_bar(aes(x=Pais, weight=Total, fill = factor(Pais))) + geom_text(aes(x=Pais, y=Total, label=as.integer(Total)), size=2.5, color="gray14") + coord_flip() + labs(title = "MUERTES POR COVID", subtitle = "Julio y Agosto 2020 + Enero y Febrero 2021", caption = "Fuente: Wikipedia", fill= "cantidad",x="Pais",y="Cantidad") + theme_classic()

#Dentro de los paises de interes, es posible observar Estados Unidos fue el que más muertes tuvo tanto en invierno como en verano.


#A continuación procederé a investigar el país que tuvo la mayor cantidad de casos a lo largo del 2020. 

#Para empezar limpiaré la tabla y agregaré una nueva columna que calcule el total de muertes por paises. 

Casos2020.tabla3 <- Casos2020.tabla [c(1,3:14)]

Casos2020.tabla3 <- Casos2020.tabla3 %>% 
  mutate(Ene = as.factor(Ene), Feb = as.factor(Feb), Mar = as.factor(Mar), Abr = as.factor(Abr), May = as.factor(May), Jun = as.factor(Jun), Jul = as.factor(Jul), Ago = as.factor(Ago), Sep = as.factor(Sep), Oct = as.factor(Oct), Nov = as.factor(Nov), Dic = as.factor(Dic))

Casos2020.tabla3<- Casos2020.tabla3 %>% 
  mutate(Ene = as.numeric(gsub(",", "", Ene)), Feb = as.numeric(gsub(",", "", Feb)), Mar = as.numeric(gsub(",", "", Mar)),  Abr = as.numeric(gsub(",", "", Abr)),  May = as.numeric(gsub(",", "", May)),  Jun= as.numeric(gsub(",", "", Jun)), Jul = as.numeric(gsub(",", "", Jul)),  Ago = as.numeric(gsub(",", "", Ago)),  Sep = as.numeric(gsub(",", "", Sep)),  Oct = as.numeric(gsub(",", "", Oct)),  Nov = as.numeric(gsub(",", "", Nov)),  Dic = as.numeric(gsub(",", "", Dic)))

names (Casos2020.tabla3) [1]= c("Pais")

Casos2020.tabla3 <- mutate(Casos2020.tabla3, Total=Ene+Feb+Abr+May+Jun+Jul+Ago+Sep+Oct+Nov+Dic)

#A continuación hare un filtro para quedarme con los paises y el total de casos. 

Filtro.paises.covid <- select(Casos2020.tabla3, Pais, Total)

#Ahora cargaré el shapefile con los pasises del mundo y luego lo mapearé.  

paises <- read_sf("Paises_Mundo.shp")

ggplot(paises)+
  geom_sf(fill="gray75", color="white") + labs(title = "Paises", caption = "Fuente: http://tapiquen-sig.jimdofree.com. Carlos Efraín Porto Tapiquén. Geografía, SIG y Cartografía Digital. Valencia, España, 2020.")+ theme_void()

#A continuación uniré los datos de las muertes por Covid a nivel mundial en 2020 y el mapa por paises. 

#Para eso primero modificaré las columnas para poder unirlos por paises.

names (paises)[1] = "Pais"

geo.covid.paises <- paises %>%
  left_join(Filtro.paises.covid, by=c("Pais"="Pais"))

covid.paises <- arrange(geo.covid.paises, Pais)

#Ahora rerealizaré un mapa que permita observar en qué partes del mundo y paises hubo más casos a lo largo del 2020. 

ggplot()+ geom_sf(data=paises)+
  geom_sf(data=covid.paises, aes(fill=Total), color="white") + labs(title = "Muertes por Covid", subtitle = "2020", caption = "Fuente: Wikipedia", x = "LATITUD", y = "LONGITUD") +  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_light()


#Por último, analizaré cuál fue el mes con mayores muertes en Argentina.

#Nuevamente limpiaré el dataset

Filtro.Argentina.Covid.2020 <- filter(Casos2020.tabla3, Pais== "Argentina")

Argentina.Covid.2020 <- gather(Filtro.Argentina.Covid.2020, key="mes", value="muertes", Ene:Dic)

#Ahora lo mapearé

Argentina.Covid.2020 %>%
  tail(10) %>%
  ggplot( aes(x=mes, y=muertes)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  theme_ipsum() + labs(title="MUERTES POR COVID", subtitle = "Argentina 2020", x="Mes", y="Muertes", caption="Fuente: Wikipedia")

#Es posible observar que en Argentina el es de Diciembre 2020 fue el que tuvo el mayor índice de contagios. 


#A modo de conclusión final, compararé la cantidad de muertes por mes de Estados Unidos y Argentina. 

#Pero antes volveré a limpiar el dataset. 

Filtro.EEUUyArgentina.Covid.2020 <- filter(Casos2020.tabla3, Pais %in% c("Estados Unidos","Argentina"))

EEUU.Argentina.Covid.2020 <- gather(Filtro.EEUUyArgentina.Covid.2020, key="mes", value="muertes", Ene:Dic)

#A continuación mapeare los datos.

ggplot(EEUU.Argentina.Covid.2020) +
  geom_point(aes(x=mes, y=muertes, color=muertes), alpha=0.5) +
  facet_grid(Pais~.) +
  scale_color_viridis_c(option="magma")

#A modo de cierre, es posible observar que en ambos paises aumentaron las muertes en 2020 hacia fin de año, es decir en noviembre y diciembre. 

#En este sentido, es probable que el aumento de los contagios y por ende muertes este más relacionado con fin de año, las festividades, el traslado de personas que esto implica, y la apertura de fronteras que a la época del año. 



