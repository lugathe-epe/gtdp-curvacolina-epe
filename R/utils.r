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

geragrade <- function(colina, nhl, npot) {

    grade <- expand.grid(
        hl  = colina$CC[, seq(min(hl),  max(hl),  length.out = nhl)],
        pot = colina$CC[, seq(min(pot), max(pot), length.out = npot)]
    )

    grade <- as.data.table(grade)

    return(grade)
}