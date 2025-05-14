#####################################################
### script para processar informacoes importantes ###
#####################################################

# pacotes necessarios

library(RPostgreSQL)
library(tidyverse)
theme_set(theme_bw())
library(lubridate)
library(rvest)
library(stringi)

####################################################################
# lista de cidades - nao achei no IBGE e estou baixando da Wikipedia

url <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_popula%C3%A7%C3%A3o"

pagina <- url %>%
  read_html() %>%
  html_table(fill = TRUE)

tabela <- pagina[[1]]

tabela %>%
  mutate(codigo = `Código IBGE`, 
         municipio = Município, 
         uf = `Unidade federativa`,
         populacao = População,
         # coluna com as ufs sem acento, letra maiuscula ou espaco
         ufTabela = stri_trans_general(str = tolower(gsub(" ", "", uf)), 
                                       id = "Latin-ASCII")) %>% 
  select(codigo, municipio, uf, ufTabela, populacao) %>%
  group_by(uf) %>%
  arrange(municipio) %>% # coloca municipios em ordem alfabetica
  write_csv("dados/codigoMunicipios.csv")



################################################################
# lista de morbidades - nao achei uma boa organizacao no datasus
# e baixei do site https://cid10.com.br/

#url_datasus     <- "http://tabnet.datasus.gov.br/cgi/sih/mxcid10lm.htm"
#url_medicinanet <- "http://www.medicinanet.com.br/categorias/lista_cid10.htm"

url <- "https://cid10.com.br/buscadescr?query="

pagina <- url %>%
  read_html() %>%
  html_table(fill = TRUE)

tabela <- pagina[[1]] %>%
  filter_all(any_vars(!is.na(.))) # remove linhas com NA em todas as colunas

# o codigo completo leva 14.31 minutos (858.89 segundos) pra rodar

for (j in 2:831){
  url <- paste0("https://cid10.com.br/buscadescr?query=&page=", j)
  
  pagina <- url %>%
    read_html() %>%
    html_table(fill = TRUE)
  
  tabela <- rbind(tabela, filter_all(pagina[[1]], any_vars(!is.na(.))))
  
}

write_csv(tabela, "dados/codigoMorbidadesCID10.csv")


