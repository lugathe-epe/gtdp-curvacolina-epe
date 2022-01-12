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

thinplate <- function(colina, taxa_reducao = 2) {

    hl <- pot <- rend <- NULL

    vec_reduz <- rep(FALSE, taxa_reducao)
    vec_reduz[1] <- TRUE

    colreduzida <- copy(colina$CC)
    colreduzida <- split(colreduzida, colreduzida$rend)
    colreduzida <- lapply(colreduzida, function(d) {
        if(nrow(d) > 10) {
            d <- d[rep(vec_reduz, length.out = .N)]
        }
        d
    })
    colreduzida <- as.curvacolina(rbindlist(colreduzida))

    nknots <- nrow(colreduzida$CC)
    knots  <- lapply(seq(nknots), function(i) colreduzida$CC[i, list(hl, pot)])

    mod <- gam(rend ~ s(hl, pot, bs = "tp", k = nknots, sp = 0, xt = list(max.knots = nknots)),
        data = colreduzida$CC, knots = knots)

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
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return vetor de rendimentos interpolados
#' 
#' @export

predict.thinplate <- function(object, pontos, ...) {

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    pontos <- as.data.frame(pontos)

    interp <- unname(predict(object$superficie, newdata = pontos))

    return(interp)
}