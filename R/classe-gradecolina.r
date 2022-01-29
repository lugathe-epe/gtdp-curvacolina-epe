########################### FUNCOES RELATIVAS AO USO E EXTRACAO DE GRADES ##########################

#' Objetos \code{gradecolina}
#' 
#' Tabelas regulares amostradas de um objeto \code{\link{interpolador}}.
#' 
#' A chamada do metodo \code{predict} em objetos da classe \code{interpolador} e suas subclasses
#' pode ser retornada de forma mais completa. Esta forma consiste na tabela regularmente espacada
#' para representacao discreta da curva colina.
#' 
#' Objetos do tipo \code{gradecolina} sao uma lista de dois elementos, o primeiro dos quais um
#'     data.table com as colunas
#' 
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia gerada}
#' \item{\code{rend}}{rendimento interpolado}
#' \item{\code{inhull}}{booleano indicando se o ponto foi interpolado (\code{TRUE}) ou extrapolado (\code{FALSE})}
#' }
#' 
#' O segundo elemento e um objeto \code{curvacolina}, contendo a colina original.
#' 
#' @name gradecolina
#' 
#' @family gradecolina
NULL

#' Construtor Interno De \code{gradecolina}
#' 
#' Funcao interna do pacote, nao deve ser chamada pelo usuario
#' 
#' @param pontos data.frame-like contendo coordenadas interpoladas
#' @param rends vetor numerico de rendimentos interpolados
#' @param interpolador tipo de interpolador de onde foi extraida
#' 
#' @return objeto da classe \code{gradecolina}; lista de dois elementos, o primeiro dos quais um
#'     data.table com as colunas
#' 
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia gerada}
#' \item{\code{rend}}{rendimento interpolado}
#' \item{\code{inhull}}{booleano indicando se o ponto foi interpolado (\code{TRUE}) ou extrapolado (\code{FALSE})}
#' }
#' 
#' O segundo elemento e um objeto \code{curvacolina}, contendo a colina original
#' 
#' @importFrom geometry inhulln convhulln

new_gradecolina <- function(pontos, rends, interpolador) {

    colina <- getcolina(interpolador)

    nhl  <- length(unique(pontos[, "hl"]))
    npot <- length(unique(pontos[, "pot"]))

    grade <- as.data.table(cbind(pontos, rend = rends))

    inhull <- inhulln(convhulln(colina$CC[, list(hl, pot)]), data.matrix(pontos))
    grade$inhull <- inhull

    out <- list(grade = grade, colina = colina)
    class(out) <- "gradecolina"
    attr(out, "interp") <- class(interpolador)[1]
    attr(out, "nhl")  <- nhl
    attr(out, "npot") <- npot

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

#' Interpolacao Bilinear
#' 
#' Interpolacao bilinear de \code{pontos} na grade bivariada \code{gradecolina}
#' 
#' \code{predict.gradecolina} interpola pontos arbitrarios especificados atraves do argumento
#' \code{pontos}. \code{fitted.gradecolina} interpola os pontos da propria curva colina original
#' na grade. \code{residuals.gradecolina} retorna os erros de interpolacao dos pontos da colina
#' original na grade.
#' 
#' @param object objeto da classe \code{gradecolina}
#' @param pontos data.frame-like contendo as coordenadas \code{hl} e \code{pot} dos pontos a 
#'     interpolar.
#' @param full.output booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um data.table de \code{pontos} com
#'     a coluna \code{rend} adicionada
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @examples
#' 
#' # usando o interpolador de triangulacao
#' tri <- interpolador(colinadummy, "tri")
#' 
#' # extrai uma grade dele
#' coord <- geragrade(colinadummy, 10, 10)
#' gradecolina <- predict(tri, coord, as.gradecolina = TRUE)
#' 
#' # interpolando pontos arbitrarios
#' coord_interp <- geragrade(colinadummy, 25, 25)
#' pred <- predict(gradecolina, coord_interp)
#' pred <- predict(gradecolina, coord_interp, full.output = TRUE)
#' 
#' # interpolando a propria curva colina
#' fitt <- fitted(gradecolina)
#' 
#' # residuos
#' resid <- residuals(gradecolina)
#' 
#' @return set \code{full.output = FALSE} vetor de rendimentos interpolados, do contrario um 
#'     \code{data.table} contendo \code{pontos} adicionado da coluna \code{rend} com resultado da
#'     interpolacao
#' 
#' @name interpolacao_bilinear
#' 
#' @family gradecolina
#' 
#' @import data.table
#' 
#' @export

predict.gradecolina <- function(object, pontos, full.output = FALSE, ...) {

    hl <- pot <- NULL

    gradecolina <- as.data.table(object[[1]])
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

#' @rdname interpolacao_bilinear
#' 
#' @export

fitted.gradecolina <- function(object, full.output = FALSE, ...) {

    hl <- pot <- NULL

    fitt <- predict(object, object$colina$CC[, list(hl, pot)], full.output)

    return(fitt)
}

#' @rdname interpolacao_bilinear
#' 
#' @export

residuals.gradecolina <- function(object, ...) {

    obs  <- object$colina$CC$rend
    prev <- fitted(object)
    res <- obs - prev

    return(res)
}

#' Escrita De \code{gradecolina}
#' 
#' Metodo para facilitacao de escrita de \code{gradecolina} lida pelas funcoes do pacote
#' 
#' @param x objeto \code{gradecolina} a ser escrito
#' @param file caminho para escrita com extensao de aquivo
#' 
#' @return Escreve grade em \code{x} no caminho especificado
#' 
#' @family gradecolina
#' 
#' @import data.table
#' 
#' @export

write.gradecolina <- function(x, file) fwrite(x$grade, file, quote = FALSE, sep = ";")
