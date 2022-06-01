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

triangulacao <- function(colina, tessfunc = tessdelaunay) {
    hl <- pot <- NULL

    tri <- tessfunc(colina)
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

# FUNCOES DE TRIANGULACAO --------------------------------------------------------------------------

#' Triangulacao Delauney
#' 
#' Realiza triangulacao do espaco pelo metodo de Delauney via \code{geometry::delaunayn}
#' 
#' @param colina objeto \code{curvacolina} contendo curva a tesselar
#' 
#' @return matriz com tres colunas indicando, em cada linha, o indice em \code{colina$CC} dos pontos
#'     correspondentes aos vertices de cada triangulo gerado

tessdelaunay <- function(colina) {
    hl <- pot <- NULL
    geometry::delaunayn(colina$CC[, list(hl, pot)])
}

#' Triangulacao Radial
#' 
#' Define os triangulos sempre usando o maximo como um dos vertices
#' 
#' Este e um metodo de triangulacao especifico para uso em torno do maximo. Cada triangulo e 
#' definido usando sempre o maximo como um dos vertices. Os dois vertices restantes sao pontos 
#' adjacentes entre si na curva de rendimento imediatamente inferior ao maximo, ou seja, o espaco 
#' entre ultima curva e ponto maximo e fatiado em triangulos com arestas radiais, para todos os 
#' pontos da ultima curva.
#' 
#' @param dat um data.frame ou data.table contendo queda liquida, potencia e rendimento
#' 
#' @return matriz de tres colunas indicando o indicie em \code{dat} dos pontos correspondentes aos
#'     vertices de cada triangulo. Cada linha corresponde a um triangulo

tessradial <- function(colina) {

    tri <- tessdelaunay(colina)

    # identifica triangulos da ultima curva de rend para dentro
    ultrends <- tail(attr(colina, "rends"), 2)
    innertri <- sapply(seq(ncol(tri)), function(i) colina$CC$rend[tri[, i]] %in% ultrends)
    innertri <- rowMeans(innertri) == 1
    tri <- tri[!innertri, ]

    dat <- copy(colina$CC)

    # centraliza o dado em torno do maximo -- isso e necessario pra que o angulo relativo a cada 
    # ponto seja o angulo entre o eixo x e a linha que passa do maximo a cada ponto da ultim curva
    dat$hl  <- dat$hl - dat[rend == ultrends[2], hl]
    dat$pot <- dat$pot - dat[rend == ultrends[2], pot]

    dat <- dat[rend %in% ultrends[1]]
    dat[, ang := atan2(pot, hl)]

    angord <- order(dat$ang)

    N1 <- nrow(dat)
    N2 <- nrow(colina$CC[!(rend %in% ultrends)])
    N3 <- nrow(colina$CC)

    out <- cbind(angord, c(angord[-1], angord[1])) + N2
    out <- cbind(out, N3)
    out <- rbind(tri, out)

    return(out)
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
#' @param as.gradecolina booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um objeto \code{gradecolina}. Veja
#'     \code{\link{gradecolina}}
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return se \code{as.gradecolina = FALSE}, vetor de rendimentos interpolados, do contrario um 
#'     objeto \code{\link{gradecolina}}
#' 
#' @export

predict.triangulacao <- function(object, pontos, as.gradecolina = FALSE, ...) {

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    npontos <- nrow(pontos)
    pontos <- data.matrix(pontos)

    triangulos <- object$triangulos
    colina     <- data.matrix(object$colina$CC)

    barycoord <- geometry::tsearchn(colina[, c("hl", "pot")], triangulos, pontos)

    rends <- sapply(seq(npontos), function(i) {
        indtri <- barycoord$idx[i]
        vertices <- triangulos[indtri, ]
        rends  <- colina[vertices, "rend"]
        sum(barycoord$p[i, ] * rends)
    })

    if(as.gradecolina) {
        out <- new_gradecolina(pontos, rends, object)
    } else {
        out <- as.numeric(rends)
    }

    return(out)
}