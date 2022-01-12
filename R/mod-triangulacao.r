################################### INTERPOLACAO VIA TRIANGULACAO ##################################

#' Triangulacao De \code{curvacolina}
#' 
#' Funcao interna executada quando \code{metodo = "triangulacao"} em \code{interpolador}
#' 
#' Esta funcao nao deve ser chamada pelo usuario diretamente na maioria dos casos
#' 
#' @param colina objeto \code{curvacolina} retornado pelas funcoes de leitura
#' 
#' @return objeto da classe \code{triangulacao} contendo a tesselacao da curva colina
#' 
#' @export

triangulacao <- function(colina) {
    tri <- geometry::delaunayn(colina$CC[, .(hl, pot)])
    new_triangulacao(tri, colina)
}

new_triangulacao <- function(tri, colina) {

    obj <- list(triangulos = tri, colina = colina)

    class(obj) <- c("triangulacao", "interpolador")
    attr(obj, "ntri") <- nrow(tri)

    return(obj)
}

#' @export

print.triangulacao <- function(x, ...) {
    cat("* Tesselacao ", "\n")
    cat("Numero de triangulos: ", attr(x, "ntri"), "\n")
    cat("-----\n")
    cat("* Curva colina \n")
    summary(x$colina)
}

# METODOS ------------------------------------------------------------------------------------------

#' @rdname getcolina

getcolina.triangulacao <- function(object) object$colina

#' Amostragem De Pontos Na Triangulacao
#' 
#' Realiza interpolacao baricentrica de \code{pontos} nos triangulos da tesselacao
#' 
#' @param object objeto da classe \code{triangulacao} retornado pela funcao homonima
#' @param pontos data.frame ou matriz contendo coordenadas \code{(hl, pot)} dos pontos onde 
#'     interpolar
#' 
#' @return vetor de rendimentos interpolados
#' 
#' @export
 
predict.triangulacao <- function(object, pontos, ...) {

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    npontos <- nrow(pontos)
    pontos <- data.matrix(pontos)

    triangulos <- object$triangulos
    colina     <- data.matrix(object$colina$CC)

    barycoord <- geometry::tsearchn(colina[, c("hl", "pot")], triangulos, pontos)

    interp <- sapply(seq(npontos), function(i) {
        indtri <- barycoord$idx[i]
        vertices <- triangulos[indtri, ]
        rends  <- colina[vertices, "rend"]
        sum(barycoord$p[i, ] * rends)
    })

    return(interp)
}