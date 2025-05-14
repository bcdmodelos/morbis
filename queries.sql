#######################################################
### exemplos de queries que podem ser feitas no sql ###
### caso em que as tabelas sao os estados           ###
#######################################################

/* morbidades mais comuns em rio branco - ac */

select linhaa, count(linhaa)
  from acre
  where codmunres = 1200401
  group by linhaa
  order by count(linhaa) desc

/* exemplo de query do app com expressao regular */

select idade, dtobito, sexo, racacor, codmunres, linhaa 
  from acre
  where codmunres = 1200401 and linhaa ~* R09

/* neoplasia maligna da mama em natal */

select idade, dtobito, sexo, racacor, codmunres, linhaa 
  from riograndedonorte
  where codmunres = 2408102 and linhaa ~* 'C50'

/* morbidades mais comuns em natal */

select linhaa, count(linhaa)
  from riograndedonorte
  where codmunres = 2408102
  group by linhaa
  order by count(linhaa) desc

/* numero de c43 em Natal em 2005 */

select linhaa, dtobito
  from riograndedonorte
  where codmunres = 2408102 and dtobito ~* '2005' and linhaa ~* 'C43'

/* numero de c43 em Sao Paulo em todo o periodo - 29 segundos */

select linhaa, dtobito
  from saopaulo
  where codmunres = 3550308 and linhaa ~* 'C43'

/* quantidade de mortes por tipo de morte violenta */

select circobito, count(circobito)
  from riograndedonorte
  group by circobito
  order by count(circobito) desc


#######################################################
### exemplos de queries que podem ser feitas no sql ###
### caso em que as tabelas sao as regioes           ###
#######################################################

/* numero de c43 em Sao Paulo em todo o periodo - 32 segundos */

select linhaa, dtobito
  from sudeste
  where codmunres = 3550308 and linhaa ~* 'C43'









