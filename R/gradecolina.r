########################### FUNCOES RELATIVAS AO USO E EXTRACAO DE GRADES ##########################

#' Interpolacao Bilinear
#' 
#' Interpola \code{pontos} na grade bivariada \code{gradecolina}
#' 
#' \code{pontos} permite que seja passado um objeto \code{curvacolina} para facilitar a interpolacao
#' dos pontos originais numa tabela gerada por algum dos metodos de interpolacao. Do contratio, 
#' \code{pontos} deve ser um data.frame ou data.table com as colunas \code{hl} e \code{pot} contendo
#' as coordenadas para interpolar.
#' 
#' @param gradecolina data.frame-like contendo colunas \code{hl}, \code{pot} e \code{rend}
#' @param pontos \code{curvacolina} ou data.frame-like contendo as coordenadas \code{hl} \code{pot} 
#'     dos pontos a interpolar. Ver Detalhes
#' @param full.output booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um data.table de \code{pontos} com
#'     a coluna \code{rend} adicionada
#' 
#' @examples
#' 
#' # usando o interpolador de triangulacao
#' tri <- interpolador(colinadummy, "tri")
#' 
#' # extrai uma grade dele
#' coord <- geragrade(colinadummy, 10, 10)
#' gradecolina <- predict(tri, coord, full.output = TRUE)
#' 
#' \dontrun{
#' # interpolacao de pontos da colina na grade
#' interpolagrade(colinadummy, gradecolina)
#' 
#' # interpolacao retornando dado cheio
#' interpolagrade(colinadummy, gradecolina, full.output = TRUE)
#' }
#' 
#' @return set \code{full.output = FALSE} vetor de rendimentos interpolados, do contrario um 
#'     \code{data.table} contendo \code{pontos} adicionado da coluna \code{rend} com resultado da
#'     interpolacao
#' 
#' @import data.table
#' 
#' @export

interpolagrade <- function(pontos, gradecolina, full.output) UseMethod("interpolagrade", pontos)

#' @rdname interpolagrade
#' 
#' @export

interpolagrade.data.frame <- function(pontos, gradecolina, full.output = FALSE) {

    hl <- pot <- NULL

    gradecolina <- as.data.table(gradecolina)
    pontos      <- as.data.table(pontos)

    hlGrade  <- gradecolina[, unique(hl)]
    potGrade <- gradecolina[, unique(pot)]
    rendGrade <- data.matrix(dcast(gradecolina, hl ~ pot, value.var = "rend")[, -1])

    pontos[, ordem0 := seq_len(.N)]
    setorder(pontos, pot)
    hlPred  <- pontos[, hl]
    potPred <- pontos[, pot]

    interp <- INTERPBILIN(hlGrade, potGrade, rendGrade, hlPred, potPred)

    if(full.output) {
        interp <- cbind(pontos[, list(hl, pot)], rend = as.numeric(interp))
    } else {
        # a funcao em cpp retorna um vetor coluna (pro R, uma matriz N x 1)
        interp <- as.numeric(interp)
    }

    interp <- interp[order(pontos$ordem0)]

    return(interp)
}

#' @rdname interpolagrade
#' 
#' @export

interpolagrade.curvacolina <- function(pontos, gradecolina, full.output = FALSE) {
    hl <- pot <- NULL
    interpolagrade(pontos$CC[, list(hl, pot)], gradecolina, full.output)
}