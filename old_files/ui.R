navbarPage("Observatório da Saúde",
               tabPanel("Início",
                        titlePanel("Teste de painel principal"),h4(div("Testando texto segundário para a adição de mais informações", style = "color:gray")),
                        fluidPage(theme = shinytheme("darkly"),
                                  
                                  pageWithSidebar(
                                    
                                    # header com o titulo
                                    #h5(headerPanel("Painel")),
                                    titlePanel("Painel"),
                                    
                                    # sidebar com as escolhas do usuario
                                    sidebarPanel(
                                      
                                      # nomes dos estados
                                      selectInput("uf", "UF", sort(unique(codigoMunicipios$uf))),
                                      
                                      
                                      # nomes das cidades, condicional aos estados
                                      selectInput("municipio", "Município", ""),
                                      
                                      # faixa de anos
                                      sliderInput(inputId = "ano", label = "Ano (ainda não funciona)",
                                                  min = 1999, max = 2020, step = 1, value = c(1999, 2020)),
                                      
                                      # tipo de classificacao de morbidade
                                      selectInput("escolhaClassificacao", "Tipo de Classificação", c("Categoria", "Subcategoria")),
                                      
                                      
                                       selectizeInput("causabas", "Morbidade", multiple = T,choices = unique(listaMorbidadesCat), 
                                                     options = list(maxItems = 5, placeholder = 'Escreva:')),
                                      p(a(href="https://github.com", "github.com"))

                                      
                                    ),
                                    
                                    # resultados a serem exibidos
                                    
                                    mainPanel(
                                      titlePanel("Gráfico"),
                                      withSpinner(plotlyOutput("plotObitosSimples")),
                                      titlePanel("Morbidades"),
                                      verbatimTextOutput("table22"),
                                      titlePanel("Informações adicionais"),
                                      verbatimTextOutput("table3"),
                                    )
                                  )#pageWithSidebar
                        )#fluidpage
               ),
               
               navbarMenu("Comparações",
                          
                          tabPanel("Regiões"),
                          tabPanel("Estados"),
                          tabPanel("Cidades"),
                          tabPanel("Macro Região")
                          
                          
               ),
               
               tabPanel("Dados",
                        
                        p(a(href="www.google.com","Download de dados"))
                        
               ),
               
               tabPanel("Documentação",
                        
                        
               ),
               
               navbarMenu("Sobre",
                          tabPanel("Projeto",
                                   includeHTML("projeto.html")
                          ),
                          
                          
                          tabPanel("Pesquisadores",
                                   
                                   includeHTML("pesquisadores.html")
                          ),
                          
                          tabPanel("Parceiros",
                                   includeHTML("parceiros.html")
                          )
               )#sobre
               
               
               
)#navbar