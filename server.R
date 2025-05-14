server <- function(input, output, session) {
  
  
  Download_DATA  <- reactive({
    if(input$check_UF == FALSE && input$check_regiao == FALSE){
      
      
      if(is.null(input$causabas_regiao)){
        if(
          (c(dados_municipio_1_regiao()%>%
             #filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             filter(as.numeric(sexo)==1)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             #filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
             select(racacor)%>%count()%>%as.numeric()) != 0)&
          
          (c(dados_municipio_1_regiao()%>%
             #filter(gsub("[[:space:]]", "", substr(causabas,1,nchar(as.character(gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))))) == gsub("[[:space:]]", "", substr(as.character(input$causabas_regiao),1,4)))%>%
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


