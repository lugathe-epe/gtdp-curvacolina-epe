################################## INTERPOLACAO VIA TENSOR PRODUCT #################################

#' Suavizacao De \code{curvacolina} Por Produto Tensor De P-Splines
#' 
#' Funcao interna executada quando \code{metodo = "tensorprod"} em \code{interpolador}
#' 
#' Esta funcao nao deve ser chamada pelo usuario diretamente na maioria dos casos
#' 
#' @param colina objeto \code{curvacolina} retornado pelas funcoes de leitura
#' @param modo um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
#'     modelada
#' @param ... parametros extras para configuracao do modelo estimado. Ver Detalhes em 
#'     \code{\link{interpolador}}
#' 
#' @return objeto da classe \code{tensorprod} contendo a suavizacao da curva colina
#' 
#' @importFrom mgcv gam te
#' 
#' @export

tensorprod <- function(colina, modo = "pot", ...) {

    args <- list(...)

    if(is.null(args$bs)) BS <- "ps" else BS <- args$bs
    if(is.null(args$k))  K  <- NA   else K  <- args$k

    form <- paste0("rend ~ te(hl, ", modo, ", k = K, bs = BS)")
    form <- as.formula(form)

    mod <- gam(form, data = colina$CC)

    new_tensorprod(mod, colina, modo)
}

new_tensorprod <- function(mod, colina, modo) {
    obj        <- list(superficie = mod, colina = colina)
    class(obj) <- c("tensorprod", "interpolador")
    attr(obj, "modo") <- modo
    return(obj)
}

# METODOS ------------------------------------------------------------------------------------------

#' @rdname getcolina

getcolina.tensorprod <- function(object) object$colina

#' Amostragem De Pontos Na Suavizacao
#' 
#' Amostra as coordenadas especificadas via \code{pontos} na superficie suavizada
#' 
#' @param object objeto da classe \code{tensorprod} retornado pela funcao homonima
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

predict.tensorprod <- function(object, pontos, as.gradecolina = FALSE, ...) {

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    pontos <- as.data.frame(pontos)

    rends <- unname(predict(object$superficie, newdata = pontos))

    if(as.gradecolina) {
        out <- new_gradecolina(pontos, rends, object)
    } else {
        out <- as.numeric(rends)
    }

    return(out)
}