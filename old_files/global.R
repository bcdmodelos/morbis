# pacotes necessarios

library(markdown)
library(shinythemes)
library(shiny)
library(plotly)
library(RPostgreSQL)
library(tidyverse)
theme_set(theme_bw())
library(ggmap)
library(lubridate)
library(shinycssloaders)
library(readxl)

# carrega funcoes extras

source("funcoes/preparacao.R")

# leitura dos codigos dos municipios

codigoMunicipios <- read_csv(file = "dados/codigoMunicipios.csv")

# leitura dos codigos das morbidades

codigoMorbidades <- read_csv(file = "dados/codigoMorbidadesCID10.csv")

listaMorbidades  <- paste(codigoMorbidades$Codigo, codigoMorbidades$Descricao, sep = " - ")

# leitura dos codigos das morbidades - categorias

codigoMorbidadesCat <- read_excel(path = "dados/CID10_CAT.xlsx")

listaMorbidadesCat  <- paste(codigoMorbidadesCat$CAT, codigoMorbidadesCat$DESCRICAO, sep = " - ")

# colunas a serem buscadas

colunas <- c("idade, dtobito, sexo, racacor, codmunres, linhaa")

# setup da leitura do banco de dados no r

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, #Nome da instância criada anteriormente
                 dbname = "dataSusBrasil", #nome do database a ser acessado
                 host = "localhost", #IP do server que hospeda o database
                 port = 5432, #porta de entrada do server
                 user = "postgres", #nome do usuário
                 password =  "884101") # senha concedida ao usuário apontado acima

