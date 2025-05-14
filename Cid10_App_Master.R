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
#library(ggmap)
library(lubridate)
library(kableExtra)
library(janitor)
library(memoise)
library(shinycssloaders) 
library(argonDash)
#library(maptools)     
library(spdep)          
#library(cartography)    
#library(tmap)           
library(leaflet)        
library(rgdal)
library(dplyr)
library(RColorBrewer) 
library(rintrojs)
library(dashboardthemes)

#shp <- readOGR("mapabrasil-master/Mapa/.", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")
#Obitos <- read.csv("Obitos.txt")
#brasileiropg2 <- merge(shp,Obitos, by.x = "CD_GEOCUF", by.y = "N")
#Encoding(brasileiropg2$Nome) <- "UTF-8"


#pal <- colorBin("Blues",domain = NULL) 
#qpal <- colorQuantile("Blues", brasileiropg2$Obitos, n = 3)
#qpal_colors <- unique(qpal(sort(brasileiropg2$Obitos)))



library(shinyWidgets)
library(tidyr)
library(highcharter)
library(stringi)
library(readr)



Tabela_O_P_ <- read_csv("dados/Tabela_O_P_.csv")
Tabela_O_P_$TAXA =  (Tabela_O_P_$OBITOS * 100000)/(Tabela_O_P_$POPULACAO)

tabela_uf_ibge <- read_csv("dados/tabela_uf_ibge.csv")

for( i in 1:27){
  tabela_uf_ibge$estado[i]=
    
    iconv(gsub(" ", "", tolower(tabela_uf_ibge$estado[i]), fixed = TRUE),from="UTF-8",to="ASCII//TRANSLIT")
}


for( i in 1:702){
  Tabela_O_P_$estado[i]=
    
    iconv(gsub(" ", "", tolower(Tabela_O_P_[i,1]), fixed = TRUE),from="UTF-8",to="ASCII//TRANSLIT")
}


h= left_join(Tabela_O_P_, tabela_uf_ibge,by = "estado")
hh=h[,c(8,5,4)]

options(highcharter.download_map_data = TRUE)
mapdata <- get_data_from_map(download_map_data("countries/br/br-all"))


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

#####
# app

ui =
  shinyUI(fluidPage(
    introjsUI(),
    navbarPage("MorbIS",
               tabPanel("Início",
                        fluidRow(
                          #withSpinner(
                          #leafletOutput("map", height = "900")
                          sliderInput("ano", "Ano:",
                                      min = 1996, max = 2021,
                                      value = 2006),
                          column(width = 12, highchartOutput('mapp', height=750))
                          #         )
                        )
                        
               ),
               
               
               
               tabPanel("Comparações",
                        titlePanel("Painel principal"),h4(div("Utilize o menu abaixo para determinar a análise que será feita", style = "color:gray")),
                        fluidPage(theme = shinytheme("cosmo"),
                                  
                                  pageWithSidebar(
                                    
                                    
                                    titlePanel("Painel"),
                                    
                                    sidebarPanel(
                                      
                                      
                                      introBox(selectInput("regiao", "Região", sort(unique(codigoMunicipios$regiao))),
                                               data.step = 1,
                                               data.intro = "Seleciona a região do Brasil desejada"
                                      ),
                                      
                                      introBox(checkboxInput(inputId = "check_UF", label = strong("Unidade federativa"), value = FALSE),
                                               data.step = 2,
                                               data.intro = "Marca se deseja escolher uma UF dada a região escolhida"
                                      ),
                                      
                                      
                                      
                                      introBox(conditionalPanel(condition = "input.check_UF == true",
                                                                selectInput("uf", "UF", sort(unique(codigoMunicipios$uf)))
                                      ),
                                      data.step = 3,
                                      data.intro = "Seleciona a UF do Brasil desejada"
                                      ),
                                      
                                      
                                      
                                      
                                      introBox(checkboxInput(inputId = "check_regiao", label = strong("Município"), value = FALSE),
                                               data.step = 4,
                                               data.intro = "Marca se deseja escolher um município dada a região e UF escolhidas"
                                      ),
                                      
                                      
                                   
                                      introBox(conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == true)",
                                                                selectInput("municipio", "Município", "")
                                      ),
                                      data.step = 5,
                                      data.intro = "Seleciona o município do Brasil desejado"
                                      ),
                                      

                                      introBox(selectInput(inputId ="comp", "Comparações", c("Todos","Gênero","Raça","Idade")),
                                               data.step = 6,
                                               data.intro = "Seleciona se deseja comparar os obitos por Todos, Gênero, Raça, Idade"
                                      ),
                                      
                                      introBox(selectInput("escolhaClassificacao_regiao", "Tipo de Classificação", c("Categoria", "Subcategoria")),
                                               data.step = 7,
                                               data.intro = "Seleciona o tipo de classificação da morbidade"
                                      ),
                                      
                                      
                                      introBox(selectizeInput("causabas_regiao", "Morbidade", multiple = T, choices = NULL, 
                                                              options = list(maxItems = 1, placeholder = 'Escreva:')),
                                               data.step = 8,
                                               data.intro = "Seleciona a morbidade"
                                      ),
                                      p(a(href="https://github.com/mnunes/cid10/", "Código-Fonte")),
                                      
                                      actionButton("btn", "Ajuda"),
                                      downloadButton("downloadData", "Download")
                                    ),
                                    
                                    
                                    mainPanel(
                                      titlePanel(textOutput("text_grafico")),
                                      introBox( 
                                       
                                        conditionalPanel(condition = "input.check_UF == false && input.comp == 'Todos'",
                                                         plotlyOutput("plotObitosSimples_regiao"),
                                        ),
                                        conditionalPanel(condition = "input.check_UF == false && input.comp == 'Gênero'",
                                                         plotlyOutput("plotObitosSimples_regiao_sexo"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == false && input.comp == 'Todos')",
                                                         plotlyOutput("plotObitosSimples_UF"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == false && input.comp == 'Gênero')",
                                                         plotlyOutput("plotObitosSimples_UF_sexo"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == true && input.comp == 'Todos')",
                                                         plotlyOutput("plotObitosSimples"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == true && input.comp == 'Gênero')",
                                                         plotlyOutput("plotObitosSimples_sexo"),
                                        ),
                                        conditionalPanel(condition = "input.check_UF == false && input.comp == 'Raça'",
                                                         plotlyOutput("plotObitosSimples_regiao_raca"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == false && input.comp == 'Raça')",
                                                         plotlyOutput("plotObitosSimples_UF_raca"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == true && input.comp == 'Raça')",
                                                         plotlyOutput("plotObitosSimples_raca"),
                                        ),
                                        
                                        conditionalPanel(condition = "input.check_UF == false && input.comp == 'Idade'",
                                                         plotlyOutput("plotObitosSimples_regiao_idade"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == false && input.comp == 'Idade')",
                                                         plotlyOutput("plotObitosSimples_UF_idade"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == true && input.comp == 'Idade')",
                                                         plotlyOutput("plotObitosSimples_idade"),
                                        ),
                                        data.step = 9,
                                        data.intro = "Gráfico gerado a partir das opções selecionadas"
                                      ),
                                      h6(div("O gráfico mostra uma barra de ferramentas no canto superior direito.", style = "color:gray")),
                                      h6(div("Ele contém botões para download da imagem em formato png, ampliar e reduzir o gráfico, dentre outros.", style = "color:gray")),

                                      introBox( 
                                        conditionalPanel(condition = "input.check_UF == false",
                                                         #h4(div("Masculino" , style = "color:white")),
                                                         h4(htmlOutput("text_Regiaooo1")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_Regiaooo1"),
                                                         h4(htmlOutput("text_Regiaooo2")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_Regiaooo2"),
                                                         h4(htmlOutput("text_Regiaooo3")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_Regiaooo3"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == false)",
                                                         h4(htmlOutput("text_UFFF1")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_UFFF1"),
                                                         h4(htmlOutput("text_UFFF2")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_UFFF2"),
                                                         h4(htmlOutput("text_UFFF3")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_UFFF3"),
                                        ),
                                        conditionalPanel(condition = "(input.check_UF == true && input.check_regiao == true)",
                                                         h4(htmlOutput("text_Municipiooo1")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_Municipiooo1"),
                                                         h4(htmlOutput("text_Municipiooo2")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_Municipiooo2"),
                                                         h4(htmlOutput("text_Municipiooo3")),
                                                         #h5(div("Raça x Idade")),
                                                         tableOutput("table_Municipiooo3"),
                                        ),
                                        data.step = 10,
                                        data.intro = "Tabelas geradas a partir das opções selecionadas"
                                      ),

                                    )
                                  )#pageWithSidebar
                        )#fluidpage
               ),
               
               
               

               
               tabPanel("Dados",
                        titlePanel("Download de dados"),h4(div("Utilize o botão abaixo para realizar o download de todos os dados selecionados na aba COMPARAÇÕES", style = "color:gray")),
                        downloadButton("downloadData2", "Download")
                        
               ),
               
               # documentação
               
               tabPanel("Documentação", includeHTML("www/documentacao.html")),
               
               # atalhos
               
               tabPanel("Atalhos", includeHTML("www/atalhos.html")),
               
               # sobre
               
               tabPanel("Sobre", includeHTML("www/sobre.html")),
               
               
               
    )#navbar
    
    
  ))
server <- function(input, output, session) {
  
  output$mapp <- renderHighchart({
    hcmap("countries/br/br-all", data = hh[hh$ANO==as.character(input$ano),-3], value = "TAXA",
          joinBy = c("hc-a2", "sigla"), name= "Taxa de obitos",
          dataLabels = list(enabled = F),
          tooltip = list(valueDecimals = 2, valuePrefix = "")) %>%
      
      
      hc_title(text = paste("Taxa de óbitos no Brasil no ano ",as.character(input$ano))) %>%
      hc_colorAxis(dataClasses = color_classes(c(
        summary(as.numeric(unlist(as.data.frame(hh[hh$ANO=="2006",2]))))[[1]],
        summary(as.numeric(unlist(as.data.frame(hh[hh$ANO=="2006",2]))))[[2]],
        mean(na.omit(hh$TAXA)),
        summary(as.numeric(unlist(as.data.frame(hh[hh$ANO=="2016",2]))))[[5]],
        summary(as.numeric(unlist(as.data.frame(hh[hh$ANO=="2016",2]))))[[6]]),
        colors = c("#BDD7E7","#6BAED6","#3182BD","#08519C") 
      )
      ) %>% 
      hc_legend(layout = "vertical", align = "right", valueDecimals = 2) %>% 
      #hc_add_theme(hc_theme_darkunica()) %>%
      hc_mapNavigation(enabled = TRUE)
  }) 

  
  
  Download_DATA  <- reactive({
    if(input$check_UF == FALSE && input$check_regiao == FALSE){
      
      
      if(is.null(input$causabas_regiao)){
        if(
          (c(dados_municipio_1_regiao()%>%
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
          
          
          p1p <- 
            dados_municipio_1_regiao()
          
          
        }else{
          
          p1p <-  paste("Dados não disponíveis")
          
          
        }
        
        
        
        
      }else{
        
        
        if(
          (c(dados_municipio_1_regiao()%>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
          p1p <- 
            dados_municipio_1_regiao()%>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))
          
          
        }else{
          
          p1p <-  paste("Dados não disponíveis")
          
        }
        
      }
      
    } 
    
    
    if(input$check_UF == TRUE && input$check_regiao == FALSE){
      
      
      if(is.null(input$causabas_regiao)){
        if(
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
             select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
          
          
          p1p <- 
            dados_municipio_1_regiao()%>% 
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) 
        }else{
          
          p1p <-  paste("Dados não disponíveis")
          
          
        }
        
        
        
        
      }else{
        
        
        if(
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
          p1p <- 
            dados_municipio_1_regiao()%>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))
          
        }else{
          
          p1p <-  paste("Dados não disponíveis")
          
        }
        
      }
      
    } 
    
    
    if(input$check_UF == TRUE && input$check_regiao == TRUE){
      
      if(is.null(input$causabas_regiao)){
        if(
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
             select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
          
          
          p1p <- 
            dados_municipio_1_regiao()%>% 
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) 
          
          
          
        }else{
          
          p1p <-  paste("Dados não disponíveis")
          
          
        }
        
        
        
        
      }else{
        
        
        if(
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
             filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
          p1p <- 
            dados_municipio_1_regiao()%>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))
          
        }else{
          
          p1p <-  paste("Dados não disponíveis")
          
        }
        
      }
      
      
    }
    return(p1p)
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(Download_DATA()), file, row.names = FALSE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(str_replace_all(as.character(Sys.time()),":","_"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(Download_DATA()), file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$btn, {
    introjs(session)
  })
  
  
  output$text_grafico <- renderText({
    if(is.null(input$causabas_regiao)){paste("Total de óbitos do período estabelecido pelo usuário")
    }else{paste("Total de óbitos por ",input$causabas_regiao," do período estabelecido pelo usuário")}
    
  })
  
  # mapa
  
#  output$map <- renderLeaflet({
#    
#    ggg = read.csv("dados/Lista_Municipios_IBGE_Brasil.csv", sep=";")
#    
#    ggg=ggg[,c(4,7)]
#    colnames(ggg) = c("UF","Pop_2010")
#    table(is.na(ggg$Pop_2010))
#    ggg=na.omit(ggg) %>%
#      group_by(UF) %>%
#      summarise(Pop_2010 = sum(Pop_2010))
#    ggg=left_join(Obitos,ggg,by="UF")
#    Encoding(ggg$Nome) <- "UTF-8"
#    ggg$obt_pop = (ggg$Obitos * 100000)/(ggg$Pop_2010)
#    
#    qpal <- colorQuantile("Blues", ggg$obt_pop, n = 5)
#    ggg$colors = qpal(ggg$obt_pop)
#    qpal_colors <-c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")       #unique(ggg$colors)
#    
#    qpal_labs=c("0-2963",
#                "2964-3662",
#                "3663-4435",
#                "4436-4952",
#                "4953-6428")
#    
#    ggg_g <- merge(shp,ggg, by.x = "CD_GEOCUF", by.y = "UF") #FAÇA ISSO COM O BANCO DE DADOS GGG
#    Encoding(ggg_g$NM_ESTADO) <- "UTF-8"
#    
#    state_popup <- paste0("<strong>Estado: </strong>", 
#                          ggg_g$NM_ESTADO, 
#                          "<br><strong>Taxa de óbitos: </strong>", 
#                          round(ggg$obt_pop))
#    
#    
#    
#    p=(leaflet(data = ggg_g ) %>%
#         addProviderTiles("CartoDB.Positron") %>%
#         addPolygons(fillColor = ~ggg$colors, 
#                     fillOpacity = 0.8, 
#                     color = "#BDBDC3", 
#                     weight = 1, 
#                     popup = state_popup) %>%
#         addLegend(position = "topright",title = "Taxas de óbitos",colors = qpal_colors, labels = qpal_labs, opacity = 1)%>%
#         addLegend(position = "bottomleft",title = '<small>Texto de teste<br>Testando aqui texto</small>',colors =qpal_colors[1], labels = "Teste texto", opacity = 1)
#       
#    )
#    
#    
#  })
  
  
  
  # municipios
  
  outVar <- reactive({
    codigoMunicipios %>%
      filter(regiao == input$regiao) %>%
      select(uf)
  })
  
  observe({
    updateSelectInput(session, "uf", choices = outVar()
    )})
  
  
  outVar2 <- reactive({
    if (input$escolhaClassificacao_regiao == "Categoria") {
      listaMorbidadesCat
    } else {
      listaMorbidades
    }
  })
  
  # morbidades
  
  updateSelectizeInput(session, "causabas_regiao", choices = unique(listaMorbidadesCat), server = TRUE)
  
  
  outVar_municipios <- reactive({
    codigoMunicipios %>%
      filter(uf == input$uf) %>%
      select(municipio)
  })
  
  
  outVar_regiao <- reactive({
    codigoMunicipios %>%
      filter(regiao == input$regiao) %>%
      select(municipio)
  })
  
  
  
  
  
  
  
  observe({
    updateSelectInput(session, "municipio_regiao", choices = outVar_regiao()
    )})
  
  outVar2_regiao <- reactive({
    if (input$escolhaClassificacao_regiao == "Categoria") {
      listaMorbidadesCat
    } else {
      listaMorbidades
    }
  })
  
  observe({
    updateSelectInput(session, "causabas_regiao", choices = outVar2_regiao()
    )})
  
  
  
  
  output$table22_regiao<-renderPrint({ 
    
    p <- table(dados_municipio_1_regiao() %>%
                 
                 select(causabas))
    
    p
    
  })
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observe({
    updateSelectInput(session, "municipio", choices = outVar_municipios()
    )})
  
  
  
  
  
  # codigo para a atualizacao condicional das morbidades
  
  outVar2_municipios <- reactive({
    if (input$escolhaClassificacao == "Categoria") {
      listaMorbidadesCat
    } else {
      listaMorbidades
    }
  })
  
  
  
  
  
  
  causabasEscolhida <- reactive({
    paste0("'", gsub(" .*$", "",input$causabas_regiao), "'")
  })
  
  
  
  
  
  
  
  
  
  query_1_regiao <- reactive({
    paste(gsub("\\s*\\w*$", "",
               paste(" select ",colunas," from ",  
                     paste("tabelaprincipal",colapse="",sep=""),
                     tabela_1_regiao(),
                     paste(" union"),collapse=" ",sep="")
    ))
    
  })
  
  
  
  
  tabela_1_regiao <- reactive({ 
    codigoMunicipios %>%
      filter(regiao == input$regiao) %>%
      select(ufTabela) %>%
      unique()%>%
      unlist()%>%
      as.character()
  })
  
  
  
  
  m_get_query <- memoise(DBI::dbGetQuery)
  
  
  #dados_regiao <- reactive({ m_get_query(con, query_1_regiao()) })
  
  #parallel::clusterEvalQ(cl = cl, {
  dados_regiao <- reactive({m_get_query(con, query_1_regiao())})
  #})
  
  dados_municipio_1_regiao <- reactive({  dados_regiao() %>%
      preparacao()
  })
  
  
  output$text_Regiaooo1 <- renderText({
    HTML(paste(paste("Número de óbitos masculinos ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  output$table_Regiaooo1<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(1)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(1)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  }  
  
  
  
  
  
  
  
  
  
  
  output$text_Regiaooo2 <- renderText({
    HTML(paste(paste("Número de óbitos femininos ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  output$table_Regiaooo2<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(2)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(2)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  
  
  
  
  
  
  output$text_Regiaooo3 <- renderText({
    HTML(paste(paste("Número de óbitos de gênero não informado ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  output$table_Regiaooo3<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=NA), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=NA), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$text_UFFF1 <- renderText({
    HTML(paste(paste("Número de óbitos masculinos ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  output$table_UFFF1<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% 
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(1)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(1)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  }  
  
  
  
  output$text_UFFF2 <- renderText({
    HTML(paste(paste("Número de óbitos femininos ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  output$table_UFFF2<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% 
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(2)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(2)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  
  
  output$text_UFFF3 <- renderText({
    HTML(paste(paste("Número de óbitos de gênero não informado ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  output$table_UFFF3<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% 
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=NA), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=NA), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  output$text_Municipiooo1 <- renderText({
    HTML(paste(paste("Número de óbitos masculinos ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  
  
  output$table_Municipiooo1<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% 
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(1)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(1)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  
  
  
  
  
  
  output$text_Municipiooo2 <- renderText({
    HTML(paste(paste("Número de óbitos femininos ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  
  
  output$table_Municipiooo2<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% 
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(2)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=as.numeric(2)), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  
  
  output$text_Municipiooo3 <- renderText({
    HTML(paste(paste("Número de óbitos de gênero não informado ",input$causabas_regiao), "Raça x Idade", sep="<br/>"))
  })
  
  
  
  
  output$table_Municipiooo3<- function() {
    if(is.null(input$causabas_regiao)){
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        
        
        p1 <- 
          dados_municipio_1_regiao()%>% 
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=NA), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
        
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
        
      }
      
      
      
      
    }else{
      
      
      if(
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(racacor)%>%count()%>%as.numeric()) != 0)&
        
        (c(dados_municipio_1_regiao()%>%
           filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
           filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
           select(idadeNumerica)%>%count()%>%as.numeric()) != 0)){
        p1 <- 
          dados_municipio_1_regiao()%>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
          filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))))) == gsub("[[:space:]]", "", substr(input$causabas_regiao,1,4)))%>%
          select(sexo,racacor,idadeNumerica)%>%
          mutate(sexo=as.numeric(sexo))%>%
          
          mutate(racacor = as.numeric(racacor))%>%
          mutate(Raca_ = case_when(
            as.numeric(racacor)==1 ~ 'Branca',
            as.numeric(racacor)==2 ~ 'Preta',
            as.numeric(racacor)==3 ~ 'Amarela',
            as.numeric(racacor)==4 ~ 'Parda',
            as.numeric(racacor)==5 ~ 'Indigena',
            is.na(as.numeric(racacor)) ~ 'Não_informado'
          ))%>%
          
          mutate(Idade_ = case_when( 
            as.numeric(idadeNumerica)<=4   ~ '0-4',
            ((as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10))   ~ '5-10',
            ((as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20))   ~ '11-20',
            ((as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40))   ~ '21-40',
            ((as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60))   ~ '41-60',
            ((as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80))   ~ '61-80',
            as.numeric(idadeNumerica)>80 ~ '81+',
            is.na(as.numeric(idadeNumerica)) ~ 'Não_informado'
          ))%>%
          
          
          mutate(Idade_ = fct_relevel(as.factor(Idade_), c('0-4','5-10','11-20','21-40','41-60','61-80','81+','Não_informado'))) %>%
          mutate(Raca_ = fct_relevel(as.factor(Raca_), c('Amarela','Branca','Indigena','Parda','Preta','Não_informado'))) %>%
          
          semi_join(data.frame(sexo=NA), by = "sexo") %>% select(Raca_,Idade_) %>% table()%>% 
          addmargins(FUN = Total)%>%
          knitr::kable("html") %>%
          kable_styling("striped", full_width = F)
        
      }else{
        
        p1 <-  paste("Dados não disponíveis")
        
      }
      
    }
    
    
  } 
  
  
  
  
  
  
  
  
  output$plotObitosSimples_regiao <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao) ){
        p <- 
          dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          summarise(Ambos = n()) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
          geom_line() +
          labs(x = "Ano", y = "Número de óbitos", title = input$regiao)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        
        p <- ggplotly(p)
      }else{ 
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Ambos = n()) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line() +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
      }
    }
    p
  })
  
  
  
  
  
  
  output$plotObitosSimples_UF <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){ 
        p <- 
          dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
          summarise(Ambos = n()) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
          geom_line() +
          labs(x = "Ano", y = "Número de óbitos", title = input$uf)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        
        p <- ggplotly(p)
      }else{
        
        
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>% 
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Ambos = n()) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line() +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
        
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
      
    }
    p
  })
  
  
  
  
  
  output$plotObitosSimples <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      
      if(is.null(input$causabas_regiao)){
        p <-
          dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
          summarise(Ambos = n()) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))+
          geom_line() +
          labs(x = "Ano", y = "Número de óbitos", title = input$municipio)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        p <- ggplotly(p)
      }else{
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio == input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Ambos = n()) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line() +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #sexo_regiao
  output$plotObitosSimples_regiao_sexo <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          summarise(Masculino = length(na.omit(sexo[as.numeric(sexo)==1])),
                    Feminino = length(na.omit(sexo[as.numeric(sexo)==2])),
                    Não_informado = sum(is.na(as.numeric(sexo)))
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Gênero')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        p <- ggplotly(p)
      }else{ 
        
        
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(sexo)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Masculino = length(na.omit(sexo[as.numeric(sexo)==1])),
                      Feminino = length(na.omit(sexo[as.numeric(sexo)==2])),
                      Não_informado = sum(is.na(as.numeric(sexo)))
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Gênero')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  #sexo_uf
  output$plotObitosSimples_UF_sexo <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
          summarise(Masculino = length(na.omit(sexo[as.numeric(sexo)==1])),
                    Feminino = length(na.omit(sexo[as.numeric(sexo)==2])),
                    Não_informado = sum(is.na(as.numeric(sexo)))
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$uf,color='Gênero')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
        p <- ggplotly(p)
        
      }else{
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(sexo)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Masculino = length(na.omit(sexo[as.numeric(sexo)==1])),
                      Feminino = length(na.omit(sexo[as.numeric(sexo)==2])),
                      Não_informado = sum(is.na(as.numeric(sexo)))
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Gênero')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #sexo_municipios
  output$plotObitosSimples_sexo <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          summarise(Masculino = length(na.omit(sexo[as.numeric(sexo)==1])),
                    Feminino = length(na.omit(sexo[as.numeric(sexo)==2])),
                    Não_informado = sum(is.na(as.numeric(sexo)))
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))+
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$municipio,color='Gênero')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
        p <- ggplotly(p)
      }else{
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(sexo)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Masculino = length(na.omit(sexo[as.numeric(sexo)==1])),
                      Feminino = length(na.omit(sexo[as.numeric(sexo)==2])),
                      Não_informado = sum(is.na(as.numeric(sexo)))
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Gênero')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
           
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  # 1 – Branca; 2 – Preta; 3 – Amarela; 4 – Parda; 5 – Indígena. 
  
  
  
  #raca_regiao
  output$plotObitosSimples_regiao_raca <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          summarise(Branca = length(na.omit(racacor[as.numeric(racacor)==1])),
                    Preta = length(na.omit(racacor[as.numeric(racacor)==2])),
                    Amarela = length(na.omit(racacor[as.numeric(racacor)==3])),
                    Parda = length(na.omit(racacor[as.numeric(racacor)==4])),
                    Indigena = length(na.omit(racacor[as.numeric(racacor)==5])),
                    Não_informado = sum(is.na(as.numeric(racacor)))
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))+
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Raça')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        p <- ggplotly(p)
      }else{ 
        
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(racacor)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Branca = length(na.omit(racacor[as.numeric(racacor)==1])),
                      Preta = length(na.omit(racacor[as.numeric(racacor)==2])),
                      Amarela = length(na.omit(racacor[as.numeric(racacor)==3])),
                      Parda = length(na.omit(racacor[as.numeric(racacor)==4])),
                      Indigena = length(na.omit(racacor[as.numeric(racacor)==5])),
                      Não_informado = sum(is.na(as.numeric(racacor)))
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Raça')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  #raca_uf
  output$plotObitosSimples_UF_raca <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
          summarise(Branca = length(na.omit(racacor[as.numeric(racacor)==1])),
                    Preta = length(na.omit(racacor[as.numeric(racacor)==2])),
                    Amarela = length(na.omit(racacor[as.numeric(racacor)==3])),
                    Parda = length(na.omit(racacor[as.numeric(racacor)==4])),
                    Indigena = length(na.omit(racacor[as.numeric(racacor)==5])),
                    Não_informado = sum(is.na(as.numeric(racacor)))
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$uf,color='Raça')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
        p <- ggplotly(p)
        
      }else{
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(racacor)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Branca = length(na.omit(racacor[as.numeric(racacor)==1])),
                      Preta = length(na.omit(racacor[as.numeric(racacor)==2])),
                      Amarela = length(na.omit(racacor[as.numeric(racacor)==3])),
                      Parda = length(na.omit(racacor[as.numeric(racacor)==4])),
                      Indigena = length(na.omit(racacor[as.numeric(racacor)==5])),
                      Não_informado = sum(is.na(as.numeric(racacor)))
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Raça')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #raca_municipios
  output$plotObitosSimples_raca <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          summarise(Branca = length(na.omit(racacor[as.numeric(racacor)==1])),
                    Preta = length(na.omit(racacor[as.numeric(racacor)==2])),
                    Amarela = length(na.omit(racacor[as.numeric(racacor)==3])),
                    Parda = length(na.omit(racacor[as.numeric(racacor)==4])),
                    Indigena = length(na.omit(racacor[as.numeric(racacor)==5])),
                    Não_informado = sum(is.na(as.numeric(racacor)))
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))+
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$municipio,color='Raça')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        p <- ggplotly(p)
      }else{
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(racacor)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise(Branca = length(na.omit(racacor[as.numeric(racacor)==1])),
                      Preta = length(na.omit(racacor[as.numeric(racacor)==2])),
                      Amarela = length(na.omit(racacor[as.numeric(racacor)==3])),
                      Parda = length(na.omit(racacor[as.numeric(racacor)==4])),
                      Indigena = length(na.omit(racacor[as.numeric(racacor)==5])),
                      Não_informado = sum(is.na(as.numeric(racacor)))
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))  +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Raça')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  
  #-inf 47 63 73 82 +inf
  
  
  
  #idade_regiao
  output$plotObitosSimples_regiao_idade <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          summarise('0-4' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)<=4])),
                    '5-10' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10)])),
                    '11-20' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20)])),
                    '21-40' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40)])),
                    '41-60' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60)])),
                    '61-80' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80)])),
                    '81+' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)>80])),
                    'Não_informado' = sum(is.na(as.numeric(idadeNumerica)))
                    
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable)))+
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Idade')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
        p <- ggplotly(p)
      }else{ 
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(idadeNumerica)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise('0-4' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)<=4])),
                      '5-10' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10)])),
                      '11-20' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20)])),
                      '21-40' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40)])),
                      '41-60' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60)])),
                      '61-80' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80)])),
                      '81+' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)>80])),
                      'Não_informado' = sum(is.na(as.numeric(idadeNumerica)))
                      
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Idade')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  #idade_uf
  output$plotObitosSimples_UF_idade <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
          summarise('0-4' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)<=4])),
                    '5-10' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10)])),
                    '11-20' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20)])),
                    '21-40' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40)])),
                    '41-60' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60)])),
                    '61-80' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80)])),
                    '81+' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)>80])),
                    'Não_informado' = sum(is.na(as.numeric(idadeNumerica)))
                    
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$uf,color='Idade')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
        p <- ggplotly(p)
        
      }else{
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(idadeNumerica)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$uf == input$uf,1]))) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise('0-4' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)<=4])),
                      '5-10' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10)])),
                      '11-20' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20)])),
                      '21-40' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40)])),
                      '41-60' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60)])),
                      '61-80' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80)])),
                      '81+' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)>80])),
                      'Não_informado' = sum(is.na(as.numeric(idadeNumerica)))
                      
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Idade')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #idade_municipios
  output$plotObitosSimples_idade <- renderPlotly({
    # condicional para exibir um aviso caso nao haja o que plotar
    if (nrow(dados_municipio_1_regiao()) == 1){
      p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      if(is.null(input$causabas_regiao)){
        p <- dados_municipio_1_regiao() %>%
          group_by(ano) %>%
          filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
          summarise('0-4' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)<=4])),
                    '5-10' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10)])),
                    '11-20' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20)])),
                    '21-40' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40)])),
                    '41-60' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60)])),
                    '61-80' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80)])),
                    '81+' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)>80])),
                    'Não_informado' = sum(is.na(as.numeric(idadeNumerica)))
                    
          ) %>%
          reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
          ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
          geom_line(aes(color=variable)) +
          labs(x = "Ano", y = "Número de óbitos", title = input$municipio,color='Idade')+ 
          scale_color_viridis(discrete = TRUE)+
          scale_x_date(date_breaks = "1 years",
                       date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))

        p <- ggplotly(p)
      }else{
        
        
        
        
        if((gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4))%in%gsub("[[:space:]]", "", substr(c(dados_municipio_1_regiao()%>%select(causabas)%>%unique()%>%as.matrix()),1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))))&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(ano)%>%na.omit()%>%count()%>%as.numeric()) !=0)&
           (c(dados_municipio_1_regiao()%>%
              filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>% 
              filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
              select(idadeNumerica)%>%na.omit()%>%count()%>%as.numeric()) !=0)
        ){
          
          p <- 
            dados_municipio_1_regiao() %>%
            group_by(ano) %>%
            filter(codmunres %in% as.character(unique(codigoMunicipios[codigoMunicipios$municipio ==input$municipio,1]))) %>%
            filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
            summarise('0-4' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)<=4])),
                      '5-10' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>4)&(as.numeric(idadeNumerica)<=10)])),
                      '11-20' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>10)&(as.numeric(idadeNumerica)<=20)])),
                      '21-40' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>20)&(as.numeric(idadeNumerica)<=40)])),
                      '41-60' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>40)&(as.numeric(idadeNumerica)<=60)])),
                      '61-80' = length(na.omit(idadeNumerica[(as.numeric(idadeNumerica)>60)&(as.numeric(idadeNumerica)<=80)])),
                      '81+' = length(na.omit(idadeNumerica[as.numeric(idadeNumerica)>80])),
                      'Não_informado' = sum(is.na(as.numeric(idadeNumerica)))
                      
            ) %>%
            reshape2::melt(id="ano")%>%as.data.frame()%>%na.omit()%>%
            ggplot(aes(x = as.Date(ISOdate(ano, 12, 31), format = "%Y"), y = value, group=as.factor(variable))) +
            geom_line(aes(color=variable)) +
            labs(x = "Ano", y = "Número de óbitos", title = input$regiao,color='Idade')+ 
            scale_color_viridis(discrete = TRUE)+
            scale_x_date(date_breaks = "1 years",
                         date_labels = "%Y")+theme(axis.text.x = element_text(angle = 60, hjust = 1))
      
          p <- ggplotly(p)
          
          
        }else{
          
          p <- ggplot(dados_municipio_1_regiao(), aes(idade, causabas)) +
            theme_nothing() +
            annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
          p <- ggplotly(p)
          
        }
        
        
      }
    }
    p
  })
  
  
  
  
}

shinyApp(server = server, ui = ui)
