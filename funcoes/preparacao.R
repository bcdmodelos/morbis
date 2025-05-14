# funcao que prepara os dados, convertendo idades e datas, 
# dentre outras coisas

preparacao <- function(dados){

 dados <- dados %>%
    # adiciona leading zeros
    mutate(dtobito = sprintf("%08s", dtobito)) %>% 
    # converte a string para data
    mutate(dtobito = dmy(dtobito)) %>% 
    # cria a coluna com o ano da morte
    mutate(ano = year(dtobito)) %>% 
    #  i)  idade categorizada (horas, dias, meses e anos)
    mutate(idadeCategoria = as.numeric(substr(idade, start = 1, stop = 1))) %>%
    #  ii) idade nas unidades acima (ou seja, os dados de idade foram 
    #      combinados se o primeiro digito for 4 ou 5)
    mutate(idadeNumerica  = as.numeric(substr(idade, start = 2, stop = 3))) %>%
    # adiciona 100 ao valor da idade se a categoria for 5
    mutate(idadeNumerica  = ifelse(idadeCategoria == "5", 
                                   idadeNumerica+100, 
                                   idadeNumerica)) %>%
    # converte de 1, 2, 3, 4, 5 para Horas, Dias, Meses e Anos
    mutate(idadeCategoria = case_when(idadeCategoria == 1   ~ "Horas",
                                      idadeCategoria == 2   ~ "Dias",
                                      idadeCategoria == 3   ~ "Meses",
                                      idadeCategoria == 4|5 ~ "Anos"))
 # retorna o resultado da funcao   
 return(dados)
}
