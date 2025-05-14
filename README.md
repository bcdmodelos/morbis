# morbis

# bd_sim: códigos no `R` para análise de dados da CID 10 com um aplicativo no shiny
 
Descrição dos arquivos:

- `bd_sim.Rproj`: arquivo do projeto
- `comandos_extras.R`: códigos no `R` com comandos extras para a obtenção de algumas informações extras
- `dados`: pasta com dados auxiliares (não é o banco de dados postgres do datasus)
  * `codigoMorbidadesCID10.csv`: arquivo com a listagem de todas as morbidades e seus respectivos códigos no CID-10 (ver arquivo `comandos_extras.R ` para saber de onde foram baixados)
  * `codigoMunicipios.csv`: códigos do IBGE para os nomes, códigos e uf dos municípios do Brasil (ver arquivo `comandos_extras.R ` para saber de onde foram baixados)
  * `tabela_uf_ibge.csv`: relação entre uf e código do IBGE (deprecated)
- `Documentos`: pasta com os seguintes arquivos auxiliares
  * `Estrutura_SIM_para_CD_Novo.pdf`:
  * `Estrutura_SIM_para_CD.pdf`:
  * `Legislacao_PDF.pdf`:
  * `Portaria.pdf`:
  * `RELATORIO_DTB_BRASIL_MUNICIPIO.xls`:
- `exemplos_codigos`: pasta com códigos para 
  *  `comandos_iniciais_bia_teste`: testes iniciais da Beatriz
  *  `comandos_iniciais.R`: testes iniciais do Marcus
- `funcoes`: pasta com funções extras, criadas para facilitar a manutenção do código
  * `preparacao.R`: função que prepara os dados, convertendo idades e datas, dentre outras coisas
- `global.R`: definições do app shiny, como pacotes a serem carregados, arquivos extras necessários e conexão com o banco de dados
- `queries.sql`: exemplos de queries sql que podem ser criadas; pode ser usado como auxiliar para um brainstorm
- `README.md`: este arquivo que está sendo lido
- `server.R`: arquivo do `R` que processa os dados e cria os gráficos no app shiny
- `ui.R`: interface do app shiny