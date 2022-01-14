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
#' 
#' @return objeto da classe \code{thinplate} contendo a suavizacao da curva colina
#' 
#' @importFrom mgcv gam s
#' 
#' @export

thinplate <- function(colina, taxa_reducao = 1) {

    hl <- pot <- rend <- NULL

    colina_reduzida <- reduzcolina(colina, taxa_reducao)

    nknots <- nrow(colina_reduzida$CC)
    knots  <- lapply(seq(nknots), function(i) colina_reduzida$CC[i, list(hl, pot)])

    mod <- gam(rend ~ s(hl, pot, bs = "tp", k = nknots, sp = 0, xt = list(max.knots = nknots)),
        data = colina_reduzida$CC, knots = knots)

    new_thinplate(mod, colina)
}

new_thinplate <- function(mod, colina) {
    obj        <- list(superficie = mod, colina = colina)
    class(obj) <- c("thinplate", "interpolador")
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
#' @param pontos data.frame ou matriz contendo coordenadas \code{(hl, pot)} dos pontos onde 
#'     interpolar
#' @param full.output booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um data.table de \code{pontos} com
#'     as coluna \code{rend} e \code{inhull}
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return vetor de rendimentos interpolados
#' 
#' @export

predict.thinplate <- function(object, pontos, full.output = FALSE, ...) {

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    pontos <- as.data.frame(pontos)

    interp <- unname(predict(object$superficie, newdata = pontos))

    if(full.output) {
        interp <- as.data.table(cbind(pontos, rend = interp))
        inhull <- geometry::inhulln(geometry::convhulln(getcolina(object)$CC[, 1:2]), data.matrix(pontos))
        interp$inhull <- inhull
    } else {
        # o as.numeric e necessario porque predict.gam retorna 'array' e nao vetor normal
        interp <- as.numeric(interp)
    }

    return(interp)
}