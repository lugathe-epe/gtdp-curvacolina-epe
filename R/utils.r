######################################## FUNCOES UTILITARIAS #######################################

#' Gera Grade \code{dhl} x \code{dpot} No Dominio Da Curva Colina
#' 
#' Funcao auxiliar para gerar grades de pontos nos quais interpolar a curva colina
#' 
#' \code{expande} permite montar grades que vao alem dos minimos e maximos de queda e potencia 
#' observados na curva colina. Deve ser informado como um vetor de duas posicoes indicando 
#' percentuais em formato decimal (5% = 0.05), sendo a primeira posicao para queda e a segunda para
#' potencia. Para um dado valor em \code{expande}, a faixa de quedas ou potencias sera expandida
#' segundo
#' 
#' \eqn{faixa_hl = range(hl) + c(-range(hl) * expande[1], -range(hl) * expande[1])}
#' 
#' O calculo para potencia e analogo.
#' 
#' @param colina objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)
#' @param dhl,dpot numero de divisoes no eixo de queda liquida e potencia, respectivamente. Tambem
#'     podem ser fornecidos vetores indicando as posicoes das divisoes de queda e potencia
#' @param expande vetor de duas posicoes indicando percentual de expansao do dominio. Ver Detalhes
#' 
#' @examples
#' 
#' grade1 <- geragrade(colinadummy, 20, 20)
#' grade2 <- geragrade(colinadummy, 40:60, seq(150, 400, by = 10))
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

geragrade <- function(colina, dhl, dpot, expande) UseMethod("geragrade", colina)

#' @export

geragrade.data.table <- function(colina, dhl, dpot, expande = c(0, 0)) {

    hl <- pot <- NULL

    dhlvetor <- length(dhl) > 1L
    dpotvetor <- length(dpot) > 1L

    if(dhlvetor) {
        hl <- dhl
        if(!(expande[1] == 0)) {
            warning("'dhl' foi passado como vetor mas 'expande[1]' nao e nulo -- ignorando expande[1]")
        }
    } else if(!dhlvetor) {
        rangehl <- range(colina$hl)
        xhl     <- expande[1] * diff(rangehl)
        rangehl <- rangehl + c(-1, 1) * xhl
        hl <- seq(rangehl[1], rangehl[2], length.out = dhl)
    }

    if(dpotvetor) {
        pot <- dpot
        if(!(expande[2] == 0)) {
            warning("'dpot' foi passado como vetor mas 'expande[2]' nao e nulo -- ignorando expande[2]")
        }
    } else if(!dpotvetor) {
        rangepot <- range(colina$pot)
        xpot     <- expande[2] * diff(rangepot)
        rangepot <- rangepot + c(-1, 1) * xpot
        pot <- seq(rangepot[1], rangepot[2], length.out = dpot)
    }

    grade <- expand.grid(hl = hl, pot = pot)

    grade <- as.data.table(grade)

    return(grade)
}

#' @export

geragrade.data.frame <- function(colina, dhl, dpot, expande = c(0, 0)) {

    grade <- geragrade.data.table(as.data.table(colina), dhl, dpot, expande)

    return(grade)
}

#' @export

geragrade.curvacolina <- function(colina, dhl, dpot, expande = c(0, 0)) {

    grade <- geragrade(colina$CC, dhl, dpot, expande)

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