#################################### INTERPOLACAO VIA THIN PLATE ###################################

#' Suavizacao De \code{curvacolina} Thin Plate Regression Splines
#' 
#' Funcao interna executada quando \code{metodo = "thinplate"} em \code{interpolador}
#' 
#' Esta funcao nao deve ser chamada pelo usuario diretamente na maioria dos casos
#' 
#' @param colina objeto \code{curvacolina} retornado pelas funcoes de leitura
#' @param taxa_reducao inteiro indicando reducao do tamanho do historico em \code{taxa_reducao}
#'     vezes. Ver Detalhes
#' @param modo um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
#'     modelada
#' @param ... nao possui funcao, so existe para compatibilizacao com a chamada generica de 
#'     \code{\link{interpolador}}
#' 
#' @return objeto da classe \code{thinplate} contendo a suavizacao da curva colina
#' 
#' @importFrom fields Tps
#' 
#' @export

thinplate <- function(colina, taxa_reducao = 1, modo = "pot", ...) {

    hl <- pot <- rend <- NULL

    colina_reduzida <- reduzcolina(colina, taxa_reducao)

    cols <- c("hl", modo)
    mod <- Tps(colina_reduzida$CC[, .SD, .SDcols = cols], colina_reduzida$CC$rend, lambda = 0)

    new_thinplate(mod, colina, modo)
}

new_thinplate <- function(mod, colina, modo) {
    obj        <- list(superficie = mod, colina = colina)
    class(obj) <- c("thinplate", "interpolador")
    attr(obj, "modo") <- modo
    return(obj)
}

# METODOS ------------------------------------------------------------------------------------------

#' @rdname getcolina

getcolina.thinplate <- function(object) object$colina

#' Amostragem De Pontos Na Suavizacao
#' 
#' Amostra as coordenadas especificadas via \code{pontos} na superficie suavizada
#' 
#' @param object objeto da classe \code{thinplate} retornado pela funcao homonima
#' @param pontos data.frame ou matriz contendo coordenadas dos pontos onde interpolar
#' @param as.gradecolina booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um objeto \code{gradecolina}. Veja
#'     \code{\link{gradecolina}}
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return se \code{as.gradecolina = FALSE}, vetor de rendimentos interpolados, do contrario um 
#'     objeto \code{\link{gradecolina}}
#' 
#' @export
#' 

predict.thinplate <- function(object, pontos, as.gradecolina = FALSE, ...) {

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    pontos <- as.data.frame(pontos)

    rends <- unname(predict(object$superficie, x = pontos))

    if(as.gradecolina) {
        out <- new_gradecolina(pontos, rends, object)
    } else {
        out <- as.numeric(rends)
    }

    return(out)
}