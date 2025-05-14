# pacotes necessarios

library(RPostgreSQL)
library(tidyverse)
theme_set(theme_bw())
library(lubridate)

# setup da leitura do banco de dados no r

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, #Nome da instância criada anteriormente
                 dbname = "dataSusBrasil", #nome do database a ser acessado
                 host = "localhost", #IP do server que hospeda o database
                 port = 5432, #porta de entrada do server
                 user = "postgres", #nome do usuário
                 password =  "BrN_2020") # senha concedida ao usuário apontado acima

# testando a existencia da conexao

isPostgresqlIdCurrent(con)

# testando a existencia da tabela

dbExistsTable(con, "acre")

# lendo toda a tabela acre para dentro do r
# (system.time calcula o tempo de execucao do comando)

system.time(acre <- dbGetQuery(con, "select * from acre"))

# lendo as colunas sexo, idade, racacor da tabela acre para dentro do r

system.time(dtab <- dbGetQuery(con, "select sexo, idade, racacor from acre"))

# grafico de coluna da variavel sexo

dtab %>%
  select(sexo) %>%
  group_by(sexo) %>%
  count() %>%
  filter(sexo == 1 | sexo == 2) %>%
  ggplot(aes(x = sexo, y = n)) +
  geom_col()

# daria pra ter feito o grafico direto com o resultado da query, 
# sem precisar criar um data frame intermediario

dbGetQuery(con, "select sexo from acre") %>%
  select(sexo) %>%
  mutate(sexo = case_when(sexo == 1 ~ "M",
                          sexo == 2 ~ "F")) %>%
  group_by(sexo) %>%
  count() %>%
  ggplot(aes(x = sexo, y = n)) +
  geom_col()

# estou assumindo que a coluna codmunocor no sus é dada pelos códigos
# do ibge, disponiveis em 
#
# https://www.ibge.gov.br/explica/codigos-dos-municipios.php
# 
# vou usar natal e campina grande nos exemplos abaixo porque sim
#
# natal: 2408102 (cidade_1)
# campina grande: 2504009 (cidade_2)

# ler tabelas com codigos do ibge

tabela_uf_ibge <- read_csv(file = "tabela_uf_ibge.csv")

# vamos precisar usar uma tabela tipo a dos estados para os municipios,
# porque ai facilita para o usuario e para a gente nomear as cidades
# nos graficos e tabelas posteriores

cidade_1 <- "2408102" 
cidade_2 <- "2504009" 

colunas <- c("idade, 
             dtobito, 
             sexo, 
             racacor, 
             codmunocor, 
             linhaa")

# pega os dois primeiros digitos da cidade para buscar a uf em tabela_uf_ibge

tabela_1 <- tabela_uf_ibge$estado[which(tabela_uf_ibge$codigo == substr(cidade_1, start = 1, stop = 2))]
tabela_2 <- tabela_uf_ibge$estado[which(tabela_uf_ibge$codigo == substr(cidade_2, start = 1, stop = 2))]

# linhaa: CIDs informados na Linha A da DO referente ao diagnostico na Linha A da DO (causa terminal - doenca ou estado morbido que causou diretamente a morte)

linhaa <- "'*A419'" # Septicemia não especificada

# cria as queries selecionando `colunas` da `tabela` para `cidade_1` e `cidade_2` com `linhaa`

query_1 <- paste("select", colunas, 
                 "from", tabela_1, 
                 "where codmunocor =", cidade_1, "AND", "linhaa =", linhaa)
query_2 <- paste("select", colunas, 
                 "from", tabela_2, 
                 "where codmunocor =", cidade_2, "AND", "linhaa =", linhaa)

# faz a consulta

dados_cidade_1 <- dbGetQuery(con, query_1)
dados_cidade_2 <- dbGetQuery(con, query_2)

# as conversoes abaixo usando mutate poderiam ter sido feitas de uma vez so
# eu fiz varias pra ficar mais didatico aqui pra gente, mas da pra alterar 
# futuramente sem problemas

# converte codmunocor para o nome da cidade

dados_cidade_1 <- dados_cidade_1 %>%
  mutate(codmunocor = "Natal")
dados_cidade_2 <- dados_cidade_2 %>%
  mutate(codmunocor = "Campina Grande")

# converte idade para anos

dados_cidade_1 <- dados_cidade_1 %>%
  mutate(idade = as.numeric(substr(idade, start = 2, stop = 3)))
dados_cidade_2 <- dados_cidade_2 %>%
  mutate(idade = as.numeric(substr(idade, start = 2, stop = 3)))

# converte dtobito para data interpretavel para o r e cria
# uma variavel para ano

dados_cidade_1 <- dados_cidade_1 %>%
  mutate(dtobito = sprintf("%08s", dtobito)) %>% # adiciona leading zeros
  mutate(dtobito = dmy(dtobito)) %>%
  mutate(ano = year(dtobito))

dados_cidade_2 <- dados_cidade_2 %>%
  mutate(dtobito = sprintf("%08s", dtobito)) %>% # adiciona leading zeros
  mutate(dtobito = dmy(dtobito)) %>%
  mutate(ano = year(dtobito))

# grafico de linha com a evolucao da qtde de obitos

dados <- rbind(dados_cidade_1, dados_cidade_2)

dados %>%
  group_by(ano, codmunocor) %>%
  summarise(obitos = n()) %>%
  ggplot(., aes(x = ano, y = obitos, group = codmunocor, colour = codmunocor)) +
  geom_line() +
  labs(x = "Ano", y = "Numero de Obitos", colour = "Municipio")



