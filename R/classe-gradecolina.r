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
#' \item{\code{pot/vaz}}{potencia ou vazao, dependendo do \code{modo} usado no interpolador}
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
#' \item{\code{pot/vaz}}{potencia ou vazao, dependendo do \code{modo} usado no interpolador}
#' \item{\code{rend}}{rendimento interpolado}
#' \item{\code{inhull}}{booleano indicando se o ponto foi interpolado (\code{TRUE}) ou extrapolado (\code{FALSE})}
#' }
#' 
#' O segundo elemento e um objeto \code{curvacolina}, contendo a colina original
#' 
#' @importFrom geometry inhulln convhulln

new_gradecolina <- function(pontos, rends, interpolador) {

    modo <- attr(interpolador, "modo")

    hl <- pot <- vaz <- rend <- NULL

    colina <- getcolina(interpolador)

    nhl <- length(unique(pontos[["hl"]]))
    nY  <- length(unique(pontos[[modo]]))

    grade <- as.data.table(cbind(pontos, rend = rends))

    g   <- attr(interpolador$colina, "g")
    rho <- attr(interpolador$colina, "rho")
    if(modo == "pot") {
        grade[, vaz := pot / (hl * rend / 100 * rho * g) * 1e6]
    } else {
        grade[, pot := vaz * (hl * rend / 100 * rho * g) / 1e6]
    }

    inhull <- inhulln(convhulln(colina$CC[, .SD, .SDcols = c("hl", modo)]), data.matrix(pontos))
    grade$inhull <- inhull

    out <- list(grade = grade, colina = colina)
    class(out) <- "gradecolina"
    attr(out, "interp") <- class(interpolador)[1]
    attr(out, "modo") <- modo
    attr(out, "nhl") <- nhl
    attr(out, "nY") <- nY

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
#' @param pontos data.frame ou matriz contendo pontos nos quais amostrar o rendimento
#' @param full.output booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um data.table de \code{pontos} com
#'     a coluna \code{rend} adicionada
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @examples
#' 
#' # usando o interpolador de triangulacao
#' tri <- interpolador(colinadummy, "triangulacao")
#' 
#' # extrai uma grade dele
#' coord <- coordgrade(colinadummy, 10, 10)
#' gradecolina <- predict(tri, coord, as.gradecolina = TRUE)
#' 
#' # interpolando pontos arbitrarios
#' coord_interp <- coordgrade(colinadummy, 25, 25)
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
#' @importFrom geometry inhulln convhulln
#' 
#' @export

predict.gradecolina <- function(object, pontos, full.output = FALSE, ...) {

    hl <- pot <- vaz <- ordem0 <- NULL

    modo <- attr(object, "modo")

    gradecolina <- as.data.table(object[[1]])
    pontos      <- as.data.table(pontos)

    hlGrade <- unique(gradecolina[["hl"]])
    YGrade  <- unique(gradecolina[[modo]])
    form <- as.formula(paste0("hl ~ ", modo))
    rendGrade <- data.matrix(dcast(gradecolina, form, value.var = "rend")[, -1])

    pontos[, ordem0 := seq_len(.N)]
    if(modo == "pot") setorder(pontos, pot) else setorder(pontos, vaz)
    hlPred <- pontos[["hl"]]
    YPred  <- pontos[[modo]]

    interp <- INTERPBILIN(hlGrade, YGrade, rendGrade, hlPred, YPred)

    if(full.output) {
        out    <- cbind(pontos[, .SD, .SDcols = c("hl", modo)], rend = as.numeric(interp))

        # o inhulln reclama se receber uma matriz de inteiros (inacreditavelmente), entao precisa
        # somar um 0.0 para converter a matriz em floats
        pts    <- data.matrix(pontos[, .SD, .SDcols = c("hl", modo)]) + .0
        inhull <- inhulln(convhulln(object$colina$CC[, .SD, .SDcols = c("hl", modo)]), pts)

        out[, inhull := inhull]
    } else {
        # a funcao em cpp retorna um vetor coluna (pro R, uma matriz N x 1)
        out <- as.numeric(interp)
    }

    out <- out[order(pontos$ordem0)]

    return(out)
}

#' @rdname interpolacao_bilinear
#' 
#' @export

fitted.gradecolina <- function(object, full.output = FALSE, ...) {

    modo <- attr(object, "modo")
    fitt <- predict(object, object$colina$CC[, .SD, .SDcols = c("hl", modo)], full.output)

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
