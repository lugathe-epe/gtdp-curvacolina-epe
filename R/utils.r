######################################## FUNCOES UTILITARIAS #######################################

#' Gera Grade \code{dhl} x \code{dpot} No Dominio Da Curva Colina
#' 
#' Funcao auxiliar para gerar grades de pontos nos quais interpolar a curva colina
#' 
#' @param colina objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)
#' @param dhl,dpot numero de divisoes no eixo de queda liquida e potencia, respectivamente. Tambem
#'     podem ser fornecidos vetores indicando as posicoes das divisoes de queda e potencia
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

geragrade <- function(colina, dhl, dpot) UseMethod("geragrade", colina)

#' @export

geragrade.data.table <- function(colina, dhl, dpot) {

    hl <- pot <- NULL

    if(length(dhl) == 1L) hl <- colina[, seq(min(hl),  max(hl),  length.out = dhl)] else hl <- dhl
    if(length(dpot) == 1L) pot <- colina[, seq(min(pot),  max(pot),  length.out = dpot)] else pot <- dpot

    grade <- expand.grid(hl = hl, pot = pot)

    grade <- as.data.table(grade)

    return(grade)
}

#' @export

geragrade.data.frame <- function(colina, dhl, dpot) {

    grade <- geragrade.data.table(as.data.table(colina), dhl, dpot)

    return(grade)
}

#' @export

geragrade.curvacolina <- function(colina, dhl, dpot) {

    grade <- geragrade(colina$CC, dhl, dpot)

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