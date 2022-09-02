######################################## FUNCOES UTILITARIAS #######################################

#' Gera Grade \code{dhl} x \code{dpot} No Dominio Da Curva Colina
#' 
#' Funcao auxiliar para gerar grades de pontos nos quais interpolar a curva colina
#' 
#' A descricao a seguir se concentra nos argumentos associados a divisoes em queda liquida e 
#' potencia. Para todo argumento de potencia existe um analogo de vazoes caso se deseje gerar uma
#' grade neste eixo, i.e. exitem \code{dvaz} e \code{byvaz} assim como \code{dpot} e \code{bypot}.
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
#' @param dhl,dpot,dvaz numero de divisoes no eixo de queda liquida e potencia, respectivamente.
#'     Tambem podem ser fornecidos vetores indicando as posicoes das divisoes de queda e potencia
#' @param byhl,bypot,byvaz intervalo entre divisoes de queda e potencia, respectivamente; se
#'     forncecido \code{dhl} e \code{dpot} serao ignorados. Ver Detalhes
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
#' # grade em vazoes
#' colina <- learqprocit(system.file("extdata/procit_cc_original.xlsx", package = "curvacolina"))
#' grade_vaz <- coordgrade(colina[[1]], 10, dvaz = 10)
#' 
#' @return data.frame contendo coordenadas dos pontos na grade
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

coordgrade <- function(colina, dhl, dpot, dvaz, byhl, bypot, byvaz, expande) UseMethod("coordgrade", colina)

#' @export

coordgrade.data.table <- function(colina, dhl, dpot, dvaz, byhl, bypot, byvaz, expande = c(0, 0)) {

    hl <- pot <- vaz <- NULL

    is_vaz <- !missing(dvaz) || !missing(byvaz)
    is_pot <- !missing(dpot) || !missing(bypot)

    if(is_vaz & is_pot) {
        stop("Foram fornecidos argumentos para segmentacao em vazao e potencia -- escolha um")
    }

    range_hl <- gerarange(colina$hl, dhl, byhl, expande)
    if(is_vaz) {
        range_Y <- gerarange(colina$vaz, dvaz, byvaz, expande)
    } else {
        range_Y <- gerarange(colina$pot, dpot, bypot, expande)
    }

    grade <- expand.grid(hl = range_hl, Y = range_Y)
    colnames(grade)[2] <- ifelse(is_vaz, "vaz", "pot")

    grade <- as.data.table(grade)

    return(grade)
}

#' @export

coordgrade.data.frame <- function(colina, dhl, dpot, dvaz, byhl, bypot, byvaz, expande = c(0, 0)) {

    grade <- coordgrade.data.table(as.data.table(colina), dhl, dpot, dvaz, byhl, bypot, byvaz, expande)

    return(grade)
}

#' @export

coordgrade.curvacolina <- function(colina, dhl, dpot, dvaz, byhl, bypot, byvaz, expande = c(0, 0)) {

    grade <- coordgrade(colina$CC, dhl, dpot, dvaz, byhl, bypot, byvaz, expande)

    return(grade)
}

#' Gera Vetor De Secoes Num Dominio
#' 
#' Funcao auxiliar para gerar uma sequencia de pontos de segmentacao num determinado vetor
#' 
#' @param vec um vetor numerico contendo o conjunto de pontos nos quais se gera a segmentacao
#' @param divs inteiro indicando numero de divisoes. Ou este ou \code{by} deve ser informado
#' @param by intervalo entre divisoes. Ou este ou \code{divs} deve ser informado
#' @param expande vetor de duas posicoes indicando percentual de expansao do dominio
#' 
#' @examples 
#' 
#' \dontrun{
#' gerarange(1:20, 5, expande = c(0, 0))
#' # [1]  1.00  5.75 10.50 15.25 20.00
#' 
#' gerarange(1:20, by = 5, expande = c(0, 0))
#' # [1]  0  5 10 15 20
#' 
#' gerarange(1:20, by = 5, expande = c(0.1, 0.1))
#' # [1] -5  0  5 10 15 20 25
#' }
#' 
#' @return vetor de divisoes no dominio de \code{vec} de acordo com os parametros passados

gerarange <- function(vec, divs, by, expande) {
    xvec <- expande[1] * diff(range(vec))
    xvec <- range(vec) + c(-1, 1) * xvec

    if(!missing(by)) {
        if(!missing(divs)) warning("Tanto 'divs' quanto 'by' foram fornecidos -- ignorando 'divs'")

        minvec <- floor(xvec[1] / by) * by
        maxvec <- ceiling(xvec[2] / by) * by
        divs <- seq(minvec, maxvec, by = by)
    }

    divsvetor <- length(divs) > 1L

    if(divsvetor) vec <- divs else vec <- seq(xvec[1], xvec[2], length.out = divs)

    return(vec)
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
#' \code{centro_Y} ou, caso estes fiquem vazios, sera calculado como a media amostral.
#' 
#' @param dat \code{data.table} contendo vertices de um poligono cuja ordem sera determinada
#' @param centro_hl opcional, valor para centralizar \code{dat$hl}. Caso vazio usa a media 
#' @param centro_Y opcional, valor para centralizar \code{dat$Y}. Caso vazio usa a media 
#' @param centro_Y opcional, valor para centralizar \code{dat$Y}. Caso vazio usa a media
#' @param modo um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
#'     modelada 
#' 
#' @return vetor de inteiros indicando os indices dos vertices na ordenacao por angulo

orderpoly <- function(dat, centro_hl, centro_Y, modo) {

    ang <- Y <- hl <- NULL

    if(missing(centro_hl)) centro_hl <- mean(dat$hl)
    if(missing(centro_Y))  centro_Y  <- mean(dat[[modo]])

    dat$hl      <- dat$hl - centro_hl
    dat[[modo]] <- dat[[modo]] - centro_Y
    ang <- atan2(dat[[modo]], dat$hl)

    angord <- order(ang)

    return(angord)
}
