# Processamento de Curvas Colina

Pacote com funcoes facilitadoras da importacao de planilhas de curva colina, visualizacao e 
modelagem das mesmas para interpolacao. 

## Instalacao

Este pacote e privado, para uso exclusivo internamente no ONS. Desta forma, nao se encontra no CRAN 
e deve ser instalado diretamente a partir do github atraves de:

```r
# Caso a biblioteca devtools nao esteja instalada, execute install.packages("devtools") primeiro
devtools::install_github("lkhenayfis/gtdp-curvacolina")

```

**!!! IMPORTANTE !!!**

Como o repositorio e privado, e necessario configurar o R com o token de acesso correspondente. Veja
`?devtools::install_github` para mais detalhes.

## Exemplo de uso

Abaixo esta um trecho de codigo exemplificando de forma simplificada o uso das funcionalidades 
contidas neste pacote. Mais informacoes acerca das funcoes utilizadas estao disponiveis nas 
respectivas paginas de ajuda.

```r

library(curvacolina)

# leitura de uma curva colina (usando planilha embutida no pacote)
arq_colina <- system.file("extdata/colina.xlsx", package = "curvacolina")
colina     <- learqcolina(arq_colina)

# visualizacao
plot(colina)

# interpolacao numa grade 20x20 cobrindo todo o dominio da curva colina
superf <- interpolador(colina, metodo = "triangulacao")
pontos <- geragrade(colina, 20, 20)
interp <- predict(superf, pontos)

```