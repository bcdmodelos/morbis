# pacotes necessarios
#setwd("C:/Users/Neo/Desktop/cid10-master")
library(markdown)
library(shinythemes)
library(shiny)
library(plotly)
library(RPostgreSQL)
library(tidyverse)
theme_set(theme_bw())
library(viridis)
library(readxl)
library(ggmap)
library(lubridate)
library(kableExtra)
library(janitor)
library(memoise)
library(shinycssloaders) 
library(argonDash)
qua = quakes[1:2,]
qua[1,] = c(10,-50,0,0,0)
qua[2,] = c(-60,-5,0,0,0)
Total = sum
#library(maptools)
#library(spdep)
#library(cartography)
#library(tmap)
#library(leaflet)
#library(rgdal)
#library(dplyr)
library(RColorBrewer) 
library(rintrojs)
library(dashboardthemes)

# shp <- readOGR("mapabrasil-master/Mapa/.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
# Obitos <- read.csv("Obitos.txt")
# brasileiropg2 <- merge(shp,Obitos, by.x = "CD_GEOCUF", by.y = "N")
# Encoding(brasileiropg2$Nome) <- "UTF-8"


# pal <- colorBin("Blues",domain = NULL) 
# qpal <- colorQuantile("Blues", brasileiropg2$Obitos, n = 3)
# qpal_colors <- unique(qpal(sort(brasileiropg2$Obitos)))



##############################################

# carrega funcoes extras

source("funcoes/preparacao.R")

# leitura dos codigos dos municipios

codigoMunicipios <- read_csv(file = "dados/codigoMunicipios.csv")

library(readr)
regiao1 <- read.csv("dados/Lista_Municipios_IBGE_Brasil.csv", sep=";")
library(stringr)
codigo <- as.vector(as.matrix(regiao1[,3]))
regiao <- str_sub(as.matrix(regiao1[,6]), start = 8)
regioes <- data.frame(codigo,regiao)
regioes <- as_tibble(regioes)
codigoMunicipios <- merge(codigoMunicipios,regioes,by="codigo")


# leitura dos codigos das morbidades

codigoMorbidades <- read_csv(file = "dados/codigoMorbidadesCID10.csv")

listaMorbidades  <- paste(codigoMorbidades$Codigo, codigoMorbidades$Descricao, sep = " - ")

# leitura dos codigos das morbidades - categorias

codigoMorbidadesCat <- read_excel(path = "dados/CID10_CAT.xlsx")

listaMorbidadesCat  <- paste(codigoMorbidadesCat$CAT, codigoMorbidadesCat$DESCRICAO, sep = " - ")

# colunas a serem buscadas

colunas <- c("idade, dtobito, sexo, codmunres, racacor, causabas")

# setup da leitura do banco de dados no r

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, #Nome da instancia criada anteriormente
                 dbname = "dataSusBrasil", #nome do database a ser acessado
                 host = "localhost", #IP do server que hospeda o database
                 port = 5432, #porta de entrada do server
                 user = "postgres", #nome do usuario
                 password =  "884101") # senha concedida ao usuario apontado acima

# paraopcao para encontrar erros no servidor

options(shiny.sanitize.errors = FALSE)

