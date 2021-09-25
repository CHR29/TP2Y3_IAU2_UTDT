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

#En ambos casos las primeras filas no aportan datos significativos dado que busco centrarme en las muertes por covid de cada pais. En este sentido, las eliminaré. 

Casos2020.tabla <-Casos2020.tabla %>% 
  slice(5:201)

Casos2020.tabla

Casos2021.tabla <-Casos2021.tabla %>% 
  slice(4:207)

Casos2021.tabla

#El propósito del trabajo será buscar si existe una relación de muertes por Covid entre los meses más fríos y más calurosos (tomaré julio y agosto 2020 y enero y febrero 2021)
#Para eso, procederé a limpiar y a acomodar los datos.

Casos2020.tabla <- Casos2020.tabla [c(1,9:10)]

Casos2020.tabla <- Casos2020.tabla %>% 
  mutate(Jul = as.factor(Jul), Ago = as.factor(Ago))

Casos2020.tabla<- Casos2020.tabla %>% 
  mutate(Jul = as.numeric(gsub(",", "", Jul)), Ago = as.numeric(gsub(",", "", Ago)))

names (Casos2020.tabla) = c("Pais", "Julio2020", "Agosto2020")


Casos2021.tabla <- Casos2021.tabla [c(1,3:4)]

Casos2021.tabla <- Casos2021.tabla %>% 
  mutate(Ene = as.factor(Ene), Feb = as.factor(Feb))

Casos2021.tabla<- Casos2021.tabla %>% 
  mutate(Ene = as.numeric(gsub(",", "", Ene)), Feb = as.numeric(gsub(",", "", Feb)))

names (Casos2021.tabla) = c("Pais", "Enero2021", "Febrero2021")

#A continuación, comenzaré a unir las dos tablar para analizarlos con mayor profundidad. 

Casos.meses.calor.frio <- left_join(Casos2021.tabla, Casos2020.tabla, by="Pais")

Casos.meses.calor.frio <- mutate(Casos.meses.calor.frio, Total=Enero2021+Febrero2021+Julio2020+Agosto2020)

Casos.meses.calor.frio <- mutate(Casos.meses.calor.frio, TotalEyF2021=Enero2021+Febrero2021)

Casos.meses.calor.frio <- mutate(Casos.meses.calor.frio, TotalJyA2020=Julio2020+Agosto2020)

#Una vez ya unidos los datos comenzaré a calcular los porcentajes para corroborar la hipótesis: "En los meses más frios las victimas aumentan". 

Porcentaje.Casos.meses.calor.frio <- Casos.meses.calor.frio %>% 
  mutate(Porcentaje.EyF.2021= TotalEyF2021 * 100 / Total) %>% 
  mutate(Porcentaje.JyA.2020 = TotalJyA2020 * 100 / Total)

Porcentaje.Casos.meses.calor.frio

#A modo de conclusión, es posible observar que no existe tal relación. A medida que el tiempo y la pandemia continuaron avanzando, hubo más casos. 
#En otras palabras, aumentaron los casos de Coivd 19 a nivel mundial, sin importar la época del año.