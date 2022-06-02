################################### COMBINACAO DE INTERPOLADORES ###################################

#' Construtor Interno De \code{interpoladorM}
#' 
#' Funcao interna que nao deve ser chamada pelo usuario
#' 
#' @param modelos lista de interpoladores estimados
#' @param quebra numerico indicando o rendimento da curva a partir da qual troca de interpolador
#' 
#' @return objeto da classe \code{interpolador} com subclasse \code{"interpoladorM"}

new_interpoladorM <- function(modelos, quebra) {
    obj <- list(superficies = modelos)

    class(obj) <- c("interpoladorM", "interpolador")
    attr(obj, "quebra") <- quebra

    return(obj)
}

# METODOS ------------------------------------------------------------------------------------------

#' @rdname getcolina

getcolina.interpoladorM <- function(object) {
    colinas <- lapply(object$superficies, function(x) getcolina(x))
    colinas <- do.call(rbind, colinas)
    return(colinas)
}

#' Amostragem De Pontos Em \code{interpoladorM}
#' 
#' Realiza a amostragem nos dois interpoladores contidos no objeto pelos metodos especificos
#' 
#' @param object objeto da classe \code{interpoladorM}
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

predict.interpoladorM <- function(object, pontos, as.gradecolina = FALSE, ...) {

    rend <- hl <- pot <- NULL

    pontos <- pontos[complete.cases(pontos), ]

    if(nrow(pontos) == 0) return(numeric(0))

    poly   <- object$superficies[[1]]$colina[rend == attr(object, "quebra")]$CC
    angord <- orderpoly(poly)
    poly   <- poly[angord]
    inpoly <- rep(TRUE, nrow(pontos))

    # primeira camada de critica, so vai testar quais pontos estao dentro do menor retangulo que
    # envolve o poligono
    inpoly <- inpoly & ((pontos[, 1] > min(poly$hl))  & (pontos[, 1] < max(poly$hl)))
    inpoly <- inpoly & ((pontos[, 2] > min(poly$pot)) & (pontos[, 2] < max(poly$pot)))

    whichin <- which(inpoly)
    inpoly[whichin] <- as.logical(PTINPOLY(data.matrix(pontos[whichin]), data.matrix(poly[, list(hl, pot)])))

    ordem0   <- unlist(split(seq(nrow(pontos)), inpoly + 1))
    l_pontos <- split(pontos, inpoly + 1)
    rends <- mapply(object$superficies, l_pontos, FUN = function(surf, pts) predict(surf, pts), SIMPLIFY = FALSE)

    out <- unname(unlist(rends)[order(ordem0)])
    if(as.gradecolina) out <- new_gradecolina(pontos, out, object)

    return(out)
}
