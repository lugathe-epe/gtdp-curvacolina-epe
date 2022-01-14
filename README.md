
<!-- README.md is generated from README.Rmd. Please edit that file -->

# curvacolina

Pacote com funcoes facilitadoras da importacao de planilhas de curva
colina, visualizacao e modelagem das mesmas para interpolacao.

## Instalacao

Este pacote e privado, para uso exclusivo internamente no ONS. Desta
forma, nao se encontra no CRAN e deve ser instalado diretamente a partir
do github atraves de:

``` r
# Caso a biblioteca devtools nao esteja instalada, execute install.packages("devtools") primeiro
devtools::install_github("lkhenayfis/gtdp-curvacolina")
```

**\!\!\! IMPORTANTE \!\!\!**

Como o repositorio e privado, e necessario configurar o R com o token de
acesso correspondente. Veja `?devtools::install_github` para mais
detalhes.

## Exemplo de uso

Abaixo esta um trecho de codigo exemplificando de forma simplificada o
uso das funcionalidades contidas neste pacote. Mais informacoes acerca
das funcoes utilizadas estao disponiveis nas respectivas paginas de
ajuda.

``` r
library(curvacolina)

# leitura de uma curva colina (usando planilha embutida no pacote)
arq_colina <- system.file("extdata/colina.xlsx", package = "curvacolina")
colina     <- learqcolina(arq_colina)

# visualizacao
plot(colina)
```

Com o objeto `colina` lido, podemos ajustar e plotar um interpolador
sobre ele…

``` r
superf <- interpolador(colina, metodo = "thinplate")

plot(superf)
```

… e extrai uma grade em formato padronizado

``` r
# extrai uma grade regular 20x20 a partir do dominio da curva colina
pontos <- geragrade(colina, 20, 20)

# calcula rendimentos interpolados nos pontos de grade
grade <- predict(superf, pontos, full.output = TRUE)

grade
#>            hl      pot     rend
#>   1: 34.02109 119.3151 34.29555
#>   2: 35.45215 119.3151 36.07708
#>   3: 36.88321 119.3151 37.78893
#>   4: 38.31428 119.3151 39.58158
#>   5: 39.74534 119.3151 41.65958
#>  ---                           
#> 396: 55.48703 450.5660 84.82066
#> 397: 56.91810 450.5660 85.15055
#> 398: 58.34916 450.5660 85.47044
#> 399: 59.78022 450.5660 85.77856
#> 400: 61.21129 450.5660 86.09460
```
