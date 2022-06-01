################################### INTERPOLACAO VIA TRIANGULACAO ##################################

#' Triangulacao De \code{curvacolina}
#' 
#' Funcao interna executada quando \code{metodo = "triangulacao"} em \code{interpolador}. O 
#' argumento \code{tessfunc} permite controlar como o espaco projetado sera tesselado em triangulos. 
#' Atualmente ha duas opcoes implementadas no pacote:ang
#' 
#' \itemize{
#' \item{\code{\link{tessdelaunay}}}
#' \item{\code{\link{tessradial}}}
#' }
#' 
#' A primeira executa a tesselacao pelo metodo de Delaunay atraves da funcao 
#' \code{\link[geometry]{delaunayn}}. A segunda e uma versao alternativa quase igual a primeira, com
#' excessao dos triangulos definidos entre a ultima curva e o maximo. Pelo metodo de Delaunay e 
#' possivel que acontecam platos, isto e, triangulos cujos tres vertices pertencem a mesma curva.
#' \code{tessradial} interfere apenas nessa regiao, forcando para que todos os triangulos entre a 
#' ultima curva e o maximo tenham este ponto como um vertice, deixando os dois restantes como pontos
#' adjacentes na ultima curva.
#' 
#' Esta funcao nao deve ser chamada pelo usuario diretamente na maioria dos casos
#' 
#' @param colina objeto \code{curvacolina} retornado pelas funcoes de leitura
#' @param tessfunc funcao pela qual executar a tesselacao do espaco. Ver Detalhes
#' @param ... nao possui funcao, so existe para compatibilizacao com a chamada generica de 
#'     \code{\link{interpolacao}}
#' 
#' @return objeto da classe \code{triangulacao} contendo a tesselacao da curva colina
#' 
#' @export

triangulacao <- function(colina, tessfunc = tessdelaunay, ...) {
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
    chl  <- dat[rend == ultrends[2]]$hl
    cpot <- dat[rend == ultrends[2]]$pot
    dat <- dat[rend == ultrends[1]]
    angord <- orderpoly(dat, chl, cpot)

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
#' @return vetor de rendimentos interpolados
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