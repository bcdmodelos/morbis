* O app parece não estar funcionando no browser
* Ao escolher Região Nordeste e dar check em Unidade federativa, as UFs listadas não são as da Região Nordeste
* O primeiro gráfico demora muito pra carregar
* Warning: The select input "causabas_regiao" contains a large number of options; consider using server-side selectize for massively improved performance. See the Details section of the ?selectizeInput help topic
  * Consultar [https://shiny.rstudio.com/articles/selectize.html](https://shiny.rstudio.com/articles/selectize.html) para ver o que se pode fazer
* "Painel" e "Gráfico" estão desalinhados no site
* A abertura do site está muito demorada. Será que o pacote `memoise` pode ajudar?
* SQL query em paralelo
* Tempos de execução do app:

| Execução | Tempo (s) |
|---|---|
|       01 |     59.41 |
|       02 |     59.23 |
|       03 |     60.51 |