#####
# app

ui <- 
  shinyUI(fluidPage(
    introjsUI(),
    navbarPage("MorbIS",
               #tabPanel("Início",
               #         fluidRow(withSpinner(leafletOutput("map", height = "900")))
               
               #),
               
               tabPanel("Comparações",
                        titlePanel("Painel principal"), h4(div("Utilize o menu abaixo para determinar a análise que será feita", style = "color:gray")),
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
               
               tabPanel("Sobre", includeHTML("www/sobre.html"))

    )#navbar
    
    
  ))



