function(input, output, session) {
  
  #########################################
  ### atualizacao dinamica da interface ###
  #########################################
  
  # codigo para a atualizacao condicional da selecao dos municipios
  
  outVar <- reactive({
    codigoMunicipios %>%
      filter(uf == input$uf) %>%
      select(municipio)
  })
  
  observe({
    updateSelectInput(session, "municipio", choices = outVar()
    )})
  
  
  
  
  
  # codigo para a atualizacao condicional das morbidades
  
  outVar2 <- reactive({
    if (input$escolhaClassificacao == "Categoria") {
      listaMorbidadesCat
    } else {
      listaMorbidades
    }
  })
  
  observe({
    updateSelectInput(session, "causabas", choices = outVar2()
    )})
  
  
  ########################################
  ### codigos para conversoes diversas ###
  ########################################
  
  # converte municipio para codigo municipio
  
  municipio_1 <- reactive({
    codigoMunicipios %>%
      filter(uf == input$uf) %>%
      filter(municipio == input$municipio) %>%
      select(codigo) %>%
      as.character()
  })
  
  
  # encontra a tabela a ser chamada
  
  tabela_1 <- reactive({
    codigoMunicipios %>%
      filter(uf == input$uf) %>%
      filter(municipio == input$municipio) %>%
      select(ufTabela) %>%
      as.character()
  })
  
  
  
  # encontra o codigo da morbidade a ser encontrada
  # basicamente, eu uso uma expressao regular para encontrar
  # a primeira PALAVRA antes do espaco e depois adiciona os
  # caracteres necessarios para formar '*PALAVRA'
  
  
  causabasEscolhida <- reactive({
    paste0("'", gsub(" .*$", "",input$causabas), "'")
  })
  
  
  #########################################
  ### construcao e execucao das queries ###
  #########################################
  
  # cria a query para municipio 1
  
  query_1 <- reactive({
    paste("select", colunas, "from", tabela_1(), "where codmunres =", municipio_1(), "AND", "causabas ~*", causabasEscolhida(), sep = " ")
  })
  
  # realiza a consulta query_1 no banco de dados, alem de converter as colunas
  # que necessitam de conversao
  
  dados_municipio_1 <- reactive({
    dados <- dbGetQuery(con, query_1())
    # condicao para testar se nao hah casos da morbidade
    # se nao ouver, cria data frame com zeros, baseado nos
    # nomes presentes no objeto `colunas`
    if (dim(dados)[1] == 0) {
      nomesColunas <- unlist(strsplit(colunas, split = ", "))
      dados_municipio_1 <- as.data.frame(matrix(0, nrow = 1, ncol = length(nomesColunas)))
      colnames(dados_municipio_1) <- nomesColunas
      dados_municipio_1 <- dados_municipio_1 %>%
        preparacao()
    } else {
      dados_municipio_1 <- dados %>%
        preparacao()
    }
  })
  
  
  # grafico 
  
  output$plotObitosSimples <- renderPlotly({
  if (nrow(dados_municipio_1()) == 1){
      p <- ggplot(dados_municipio_1(), aes(idade, causabas)) +
        theme_nothing() +
        annotate("text", x = 0, y = 0, label = "Dados não disponíveis")
      p
    } else {
      p <- dados_municipio_1() %>%
        group_by(ano) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(x = ano, y = obitos)) +
        geom_line() +
        labs(x = "Ano", y = "Número de óbitos", title = input$municipio)
      p <- ggplotly(p)
    }
    p
  })
  

  
  
  #informações das morbidades
  output$table22<-renderPrint({ 
    
    if (nrow(dados_municipio_1()) == 1){
      p <- "Dados não disponíveis"
      p
    } else {
      p <- table(dados_municipio_1() %>%
                   select(causabas))
    }
    p
    
  })
  
  
  #Tabela dos dados
  output$table3<-renderPrint({ 
    
    if (nrow(dados_municipio_1()) == 1){
      p <- "Dados não disponíveis"
      p
    } else {
      p <- dados_municipio_1() %>%
        group_by(ano) 
      
    }
    p
    
  })
  
}

