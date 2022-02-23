library(tidyverse)
library(tidyr)
library(ggplot2)
library(readxl)
library(tmap)
library(rgdal)
library(raster)
library(maptools)
library(broom)
library(knitr)
library(kableExtra)
library(RColorBrewer)

universidades <- read.csv2("http://repositorio.dados.gov.br/educacao/CADASTRO%20DAS%20IES_2011.csv", skip = 10, header = TRUE)

universidades <- universidades[1:2365, ]

Brasil_resumo <- universidades %>% 
group_by(REGIAOIES,SIGLA, NOMEUFIES, REDE) %>% 
  count()

ggplot(Brasil_resumo, aes(x = reorder(SIGLA, n), y = n, fill = REDE)) +
  geom_col(position = "dodge") +
  labs(title = "Quantidade de Universidades por Estado",
       subtitle = "Segundo o Tipo de Rede de Ensino (2011)",
       x = "Estado",
       y = "Univresidades",
       caption = "Fonte: Instituições de Ensino Superior") +
  theme_bw()


RJ_resumo <- universidades %>% 
  filter(SIGLA == "RJ") %>% 
  group_by(SIGLA, NOMEUFIES, MUNICIPIOIES, COMUNICIES) %>% 
  count()


ggplot(RJ_resumo, aes(x = reorder(MUNICIPIOIES,n), y = n, fill = REDE)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Quantidade de Universidades por Município no Estado do Rio de Janeiro",
       subtitle = "Segundo o Tipo de Rede de Ensino (2011)",
       x = "Municípios",
       y = "Universidades",
       caption = "Fonte: Instituições de Ensino Superior") +
  theme_bw()

rj_shp <- readOGR(dsn = "RJ_shp", layer = "Base2018abril2019_equiv_albers", encoding = "UTF-8", use_iconv = TRUE)

class(RJ_resumo$COMUNICIES) = "character"

RJ_resumo$COMUNICIES <- substr(RJ_resumo$COMUNICIES, 1, 6)

RJ_resumo <- RJ_resumo %>% 
  rename("TOTAL_UNI" = "n",
         "CODMUN" = "COMUNICIES")

rj_completo <- left_join(x = rj_shp@data,
                     y = RJ_resumo,
                     by = "CODMUN") 

#rj_completo <- rj_completo %>% 
# mutate_all(replace_na, 0)

rj_shp@data <- rj_completo

view(rj_shp@data)

rj_shp_final <- rj_shp[ ,c(2:10,14)] 

view(rj_shp_final)

writeOGR(obj = rj_shp_final,
         layer = "rj_uni",
         driver = "ESRI Shapefile",
         dsn = "rj_final")

tm_shape(shp = rj_shp_final) +
  tm_polygons("TOTAL_UNI",
              title = "Tertiary Institutions",
              style = "fixed",
              palette = "Greens",
              breaks = c(1,2,5,10,Inf),
              colorNA = "white",
              textNA = "No Info") +
  tm_layout(main.title = "Number of Tertiary Institutions in the State of Rio de Janeiro in 2011",
            main.title.size = 1.3) +
  tm_compass(type = "8star",
             show.labels = 3)
         