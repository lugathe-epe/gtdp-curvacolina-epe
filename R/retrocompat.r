########################### RETROCOMPATIBILIDADE COM INTERPOLACAO ANTIGA ###########################

#' Classe \code{retamindist}
#' 
#' Construtor de objetos da classe \code{retamindist}
#' 
#' A interpolacao antiga esta implementada em VBA e nao deve ser portada para outras linguagens. A
#' partir de execucoes especiais da macro e possivel gerar uma tabela de colina interpolada tal qual
#' as funcoes fornecidas por este pacote.
#' 
#' \code{retamindist} e uma funcao de leitura para importar as tabelas geradas e encapsular o dado
#' em uma classe que se comporte de forma similar aos outros interpoladores deste pacote
#' 
#' @param arq caminho da grade gerada por interpolacao de reta com menor distancia
#' 
#' @return objeto da classe \code{interpolador} e subclasse \code{retamindist} contendo a colina
#'     interpolada pelo metodo antigo
#' 
#' @import data.table
#' 
#' @export 

retamindist <- function(arq) {

    rend <- NULL

    dat <- fread(arq)
    colnames(dat) <- c("hl", "pot", "rend")
    dat[, rend := rend * 100]

    indice <- regmatches(arq, regexpr("(_[[:digit:]]{1})", arq))

    arq_colina <- sub("Curva Colina Processada/.*", "Curva Colina Original", arq)
    arq_colina <- list.files(arq_colina, full.names = TRUE)

    if((length(indice) == 0)) {
        indice <- "_1.csv"

        if(length(arq_colina) > 1) {
            warn <- paste0("'arq' nao contem um sufixo indicando indice de colina, porem ha mais ",
                "de uma curva em 'Curva Colina Original' -- sera assumido que se refere a primeira",
                " (colina_1.csv)")
            warning(warn)
        }

        arq_colina <- arq_colina[grep(paste0(indice, "$"), arq_colina)]
    }

    colina <- fread(arq_colina)
    colina <- as.curvacolina(colina)

    new_retamindist(dat, colina)
}

new_retamindist <- function(grade, colina) {
    out <- list(superficie = grade, colina = colina)
    class(out) <- c("retamindist", "interpolador")

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------


#' @rdname getcolina

getcolina.retamindist <- function(object) object$colina

#' Amostragem De Pontos Na \code{retamindist}
#' 
#' Metodo dummy para consistencia com outras classes de interpolador
#' 
#' Os demais objetos gerados por \code{\link{interpolador}} sao modelos dos quais e possivel extrair
#' uma grade. Por outro lado, objetos \code{retamindist} \bold{ja sao interpolacoes} da curva colina
#' atraves do metodo vigente ate o Ciclo 2, implementado em VBA. Por esta razao seu \code{predict}
#' nao tem uso direto, apenas retornando a grade gerada.
#' 
#' @param object objeto da classe \code{retamindist} retornado pela funcao homonima
#' @param pontos nao tem uso neste metodo, existe apenas para consistencia com os outros
#' @param full.output booleano -- se \code{FALSE} (padrao) retorna apenas o vetor de rendimentos
#'     interpolados nas coordenadas \code{pontos}; se \code{TRUE} um data.table de \code{pontos} com
#'     as coluna \code{rend} e \code{inhull}
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return vetor de rendimentos interpolados
#' 
#' @export
 
predict.retamindist <- function(object, pontos, full.output = FALSE, ...) {

    if(full.output) {
        interp <- object$superficie
        inhull <- geometry::inhulln(geometry::convhulln(getcolina(object)$CC[, 1:2]),
            data.matrix(interp[, 1:2]))
        interp$inhull <- inhull
    } else {
        interp <- as.numeric(object$superficie$rend)
    }

    return(interp)
}