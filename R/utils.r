######################################## FUNCOES UTILITARIAS #######################################

#' Gera Grade \code{nhl} x \code{npot} No Dominio Da Curva Colina
#' 
#' Funcao auxiliar para gerar grades de pontos nos quais interpolar a curva colina
#' 
#' @param colina objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)
#' @param nhl,npot numero de divisoes no eixo de queda liquida e potencia, respectivamente
#' 
#' @examples
#' 
#' grade <- geragrade(colinadummy, 20, 20)
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

geragrade <- function(colina, nhl, npot) UseMethod("geragrade", colina)

#' @export

geragrade.data.table <- function(colina, nhl, npot) {

    if(any(is.na(colina))) {
        grade <- data.frame(hl = NA, pot = NA)
    } else {
        grade <- expand.grid(
            hl  = colina[, seq(min(hl),  max(hl),  length.out = nhl)],
            pot = colina[, seq(min(pot), max(pot), length.out = npot)]
        )
    }

    grade <- as.data.table(grade)

    return(grade)
}

#' @export

geragrade.data.frame <- function(colina, nhl, npot) {

    grade <- geragrade.data.table(as.data.table(colina), nhl, npot)

    return(grade)
}

#' @export

geragrade.curvacolina <- function(colina, nhl, npot) {

    grade <- geragrade(colina$CC, nhl, npot)

    return(grade)
}
