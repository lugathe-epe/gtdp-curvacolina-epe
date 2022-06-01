######################################## FUNCOES UTILITARIAS #######################################

#' Gera Grade \code{dhl} x \code{dpot} No Dominio Da Curva Colina
#' 
#' Funcao auxiliar para gerar grades de pontos nos quais interpolar a curva colina
#' 
#' Os argumentos \code{dhl} e \code{dpot} especificam a grade de maneira mais simples: se forem 
#' inteiros, as coordenadas de queda e potencia da grade sao geradas segmentando a faixa de quedas
#' e potencias contidas na colina em \code{dhl} e \code{dpot} respectivamente. Caso sejam vetores,
#' a grade sera construida com as posicoes contidas nestes vetores. Veja os Exemplos.
#' 
#' \code{byhl} e \code{bypot} permitem uma especificacao mais detalhada. Estes argumentos sao 
#' interpretados como o intervalo de separacao entre cada divisao de queda e potencia na grade. Para
#' manter a consistencia, a faixa de quedas e potencias usada nessa construcao nao sao exatamente 
#' aquelas contidas na colina, como quando gerando grades a partir de \code{dhl} e \code{dpot} (pois
#' muito provavelmente uma sequencia comecando em min(hl) andando de byhl em byhl nao terminaria em
#' max(hl)). Os minimos e maximos das faixas serao obtidos segundo as regras:
#' 
#' \itemize{
#' \item{minimo: o maior multiplo de \code{byX} menor que min(X)}
#' \item{maximo: o menor multiplo de \code{byX} maior que max(X)}
#' }
#' 
#' Se estes dois argumentos forem passados, \code{dhl} e \code{dpot} sao automaticamente ignorados.
#' 
#' \code{expande} permite montar grades que vao alem dos minimos e maximos de queda e potencia 
#' observados na curva colina. Deve ser informado como um vetor de duas posicoes indicando 
#' percentuais em formato decimal (5% = 0.05), sendo a primeira posicao para queda e a segunda para
#' potencia. Para um dado valor em \code{expande}, a faixa de quedas ou potencias sera expandida
#' segundo
#' 
#' \eqn{faixa_hl = range(hl) + c(-range(hl) * expande[1], -range(hl) * expande[1])}
#' 
#' O calculo para potencia e analogo. Esta expansao de faixa se aplica tanto no caso em que 
#' \code{dhl} e \code{dpot} sao passados quanto \code{byhl} e \code{bypot}. Adicionalmente, se
#' \code{dhl} e \code{dpot} forem fornecidos como vetores, \code{expande} sera ignorado.
#' 
#' @param colina objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)
#' @param dhl,dpot numero de divisoes no eixo de queda liquida e potencia, respectivamente. Tambem
#'     podem ser fornecidos vetores indicando as posicoes das divisoes de queda e potencia
#' @param byhl,bypot intervalo entre divisoes de queda e potencia, respectivamente; se forncecido
#'     \code{dhl} e \code{dpot} serao ignorados. Ver Detalhes
#' @param expande vetor de duas posicoes indicando percentual de expansao do dominio. Ver Detalhes
#' 
#' @examples
#' 
#' # grade contemplando toda a faixa de quedas e potencias da colina, cada uma divida em 20 partes
#' grade1 <- coordgrade(colinadummy, 20, 20)
#' 
#' # grade com coordenadas especificadas diretamente
#' grade2 <- coordgrade(colinadummy, 40:60, seq(150, 400, by = 10))
#' 
#' # grade com intervalos de 1 cm em queda e 10 MW em potencia
#' # grade3 <- coordgrade(colinadummy, byhl = .1, bypot = 10)
#' 
#' \dontrun{
#' plot(colinadummy, "2d") + ggplot2::geom_point(data = grade, aes(hl, pot), col = 2)
#' }
#' 
#' @return data.frame contendo coordenadas dos pontos na grade
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

coordgrade <- function(colina, dhl, dpot, byhl, bypot, expande) UseMethod("coordgrade", colina)

#' @export

coordgrade.data.table <- function(colina, dhl, dpot, byhl, bypot, expande = c(0, 0)) {

    hl <- pot <- NULL

    xhl <- expande[1] * diff(range(colina$hl))
    xhl <- range(colina$hl) + c(-1, 1) * xhl

    xpot <- expande[2] * diff(range(colina$pot))
    xpot <- range(colina$pot) + c(-1, 1) * xpot

    if(!missing(byhl)) {
        if(!missing(dhl)) warning("Tanto 'dhl' quanto 'byhl' foram fornecidos -- ignorando 'dhl'")

        minhl <- floor(xhl[1] / byhl) * byhl
        maxhl <- ceiling(xhl[2] / byhl) * byhl
        dhl <- seq(minhl, maxhl, by = byhl)
    }

    if(!missing(bypot)) {
        if(!missing(dpot)) warning("Tanto 'dpot' quanto 'bypot' foram fornecidos -- ignorando 'dpot'")

        minpot <- floor(xpot[1] / bypot) * bypot
        maxpot <- ceiling(xpot[2] / bypot) * bypot
        dpot <- seq(minpot, maxpot, by = bypot)
    }

    dhlvetor <- length(dhl) > 1L
    dpotvetor <- length(dpot) > 1L

    if(dhlvetor) hl <- dhl else hl <- seq(xhl[1], xhl[2], length.out = dhl)
    if(dpotvetor) pot <- dpot else pot <- seq(xpot[1], xpot[2], length.out = dpot)

    grade <- expand.grid(hl = hl, pot = pot)

    grade <- as.data.table(grade)

    return(grade)
}

#' @export

coordgrade.data.frame <- function(colina, dhl, dpot, byhl, bypot, expande = c(0, 0)) {

    grade <- coordgrade.data.table(as.data.table(colina), dhl, dpot, byhl, bypot, expande)

    return(grade)
}

#' @export

coordgrade.curvacolina <- function(colina, dhl, dpot, byhl, bypot, expande = c(0, 0)) {

    grade <- coordgrade(colina$CC, dhl, dpot, byhl, bypot, expande)

    return(grade)
}

# --------------------------------------------------------------------------------------------------

#' Generica Para Extrair Colina Original do Ajuste
#' 
#' Extrai o dado ajustado corretamente dependendo do tipo de modelo. Funcao interna
#' 
#' @param object objeto do qual extrair \code{data.table} da curva colina. Pode ser um modelo 
#'     interpolador ou objeto da classe \code{curvacolina}
#' 
#' @return \code{data.table} contendo a curva colina em formato padronizado

getcolina <- function(object) UseMethod("getcolina")

# --------------------------------------------------------------------------------------------------

#' Ordem Dos Vertices De Poligono
#' 
#' Centraliza \code{dat} por referencia e retorna vetor de indices dos vertices em ordem adjacente
#' 
#' A determinacao de ordem dos vertices e feita com base no angulo entre cada um e o centro do
#' poligono. Este centro pode ser infromado por meio dos argumentos \code{centro_hl} e 
#' \code{centro_pot} ou, caso estes fiquem vazios, sera calculado como a media amostral.
#' 
#' @param dat \code{data.table} contendo vertices de um poligono cuja ordem sera determinada
#' @param centro_hl opcional, valor para centralizar \code{dat$hl}. Caso vazio usa a media 
#' @param centro_pot opcional, valor para centralizar \code{dat$pot}. Caso vazio usa a media 
#' 
#' @return vetor de inteiros indicando os indices dos vertices na ordenacao por angulo

orderpoly <- function(dat, centro_hl, centro_pot) {

    if(missing(centro_hl))  centro_hl  <- mean(dat$hl)
    if(missing(centro_pot)) centro_pot <- mean(dat$pot)

    dat$hl  <- dat$hl - centro_hl
    dat$pot <- dat$pot - centro_pot
    dat[, ang := atan2(pot, hl)]

    angord <- order(dat$ang)

    return(angord)
}
