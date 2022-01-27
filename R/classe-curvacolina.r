####################################### FUNCOES PARA LEITURA #######################################

#' Le Planilha de Curva Colina
#' 
#' Funcoes para leitura de arquivos xlsx e extracao da curva colina em formato padronizado
#' 
#' \code{learqcolina} deve ser utilizada para leitura de arquivos especificos de curva colina, nos 
#' quais existe apenas uma aba contendo a informacao das curvas. \code{learqprocit} e especifica 
#' para uso com planilhas de processo iterativo, nas quais pode haver mais de uma curva colina.
#' 
#' Para \code{learqcolina}, \code{g} e \code{rho} permitem que sejam passados por fora a densidade e
#' gravidade no local. \code{learqprocit} ja busca esses valores por padrao na planilha de processo
#' iterativo, nao sendo necessario passa-los.
#' 
#' @param arq caminho da planilha
#' @param aba aba a ser lida. Por padrao igual a \code{1}, geralmente nao deve ser informado pelo 
#'     usuario
#' @param g,rho parametros opcionais contendo gravidade e densidade da agua no local. Ver Detalhes
#' 
#' @examples 
#' 
#' # arquivo de colina simples
#' arq <- system.file(package = "curvacolina", "extdata/colina.xlsx")
#' colina <- learqcolina(arq)
#' 
#' \dontrun{
#' plot(colina)
#' }
#' 
#' @return objetos \code{curvacolina}: lista de um elemento \code{data.table} contendo as colunas
#' 
#' \describe{
#' \item{\code{hl}}{queda liquida}
#' \item{\code{pot}}{potencia gerada}
#' \item{\code{vaz}}{vazao turbinada}
#' \item{\code{rend}}{rendimento correspondente}
#' }
#' 
#' Adicionalmente tem os atributos:
#' 
#' \describe{
#' \item{\code{ncurvas}}{numero de curvas na colina}
#' \item{\code{rends}}{vetor de rendimentos contidos na curva colina}
#' \item{\code{max}}{rendimento no "olho" da colina}
#' \item{\code{rho}}{densidade da agua associada}
#' \item{\code{g}}{gravidade associada}
#' }
#' 
#' \code{leaqrcolina} retorna apenas um objeto, enquanto \code{learqprocit} retornara uma lista de
#' tantos elementos quanto abas de curva colina existem na planilha \code{arq}
#' 
#' @rdname leexcel
#' 
#' @family curvacolina
#' 
#' @export

learqcolina <- function(arq, aba = 1, g, rho) {

    plan <- as.data.frame(readxl::read_xlsx(arq, aba, col_names = FALSE, .name_repair = "minimal"))

    if(nrow(plan) == 0) return(NULL)

    ncurvas <- (ncol(plan) + 1) / 3
    curvas  <- lapply(seq(ncurvas), function(i) {
        col1 <- 1 + (i - 1) * 3
        col2 <- col1 + 1

        rend <- plan[1, col1]

        curva <- plan[, col1:col2]
        curva <- curva[complete.cases(curva), ]
        curva[] <- lapply(curva, as.numeric)
        colnames(curva) <- c("hl", "pot")

        list(rend, curva)
    })

    new_curvacolina(curvas, g, rho)
}

#' @rdname leexcel
#' 
#' @export

learqprocit <- function(arq) {

    hl <- pot <- vaz <- rend <- NULL

    abas <- readxl::excel_sheets(arq)
    abas_colina   <- abas[grep("Colina", abas)]
    abas_abertura <- abas[grep("Abertura", abas)]

    alterada <- grepl("Alterada", abas_colina)
    if(any(alterada)) abas_colina <- abas_colina[alterada]

    rho_g <- lapply(abas_abertura, function(a) {
        plan <- as.data.table(readxl::read_xlsx(arq, a, col_names = FALSE, .name_repair = "minimal"))
        plan <- as.numeric(plan[14:18, 7][[1]])
        if(all(is.na(plan[4:5]))) return(plan[1:2]) else return(plan[4:5])
    })

    colinas <- mapply(abas_colina, rho_g, FUN = function(a, r_g) {
        learqcolina(arq, a, r_g[1], r_g[2])
    }, SIMPLIFY = FALSE)

    # pode ser que alguma planilha tenha sido montada errado, com abas 'Alterada' vazias
    # nesse caso refaz a leitura das abas 'Original' mesmo, emitindo um aviso
    all_null <- all(sapply(colinas, is.null))
    if(all_null) {
        abas_colina <- abas[grepl("Colina Original", abas)]
        colinas <- mapply(abas_colina, rho_g, FUN = function(a, r_g) {
            learqcolina(arq, a, r_g[1], r_g[2])
        }, SIMPLIFY = FALSE)

        warning("Abas 'Alterada' em '", arq, "' foram encontradas vazias -- leitura realizada das 'Original'")
    }

    colinas <- colinas[!sapply(colinas, is.null)]

    if(length(colinas) > 1) names(colinas) <- paste0("colina_", seq(abas_colina)) else colinas <- colinas[[1]]

    return(colinas)
}

#' @import data.table

new_curvacolina <- function(curvas, g, rho) {

    hl <- pot <- vaz <- rend <- NULL

    if(missing(rho)) rho <- NA
    if(missing(g)) g <- NA

    rends <- lapply(curvas, "[[", 1)
    rends <- sapply(rends, function(x) as.numeric(regmatches(x, regexpr("[[:digit:]]+(\\.[[:digit:]]+)?", x))))

    curvas <- lapply(curvas, "[[", 2)

    colina <- mapply(rends, curvas, FUN = function(r, c) cbind(c, rend = r), SIMPLIFY = FALSE)
    colina <- do.call(rbind, colina)
    colina <- as.data.table(colina)
    colina[, vaz := pot / (hl * rend / 100 * rho * g) * 1e6]
    setcolorder(colina, c("hl", "pot", "vaz", "rend"))

    colina <- list(CC = colina)

    temmax <- colina$CC[rend == max(rend), .N == 1]

    class(colina) <- c("curvacolina")
    attr(colina, "rends") <- rends
    attr(colina, "max") <- ifelse(temmax, colina$CC[which.max(rend), rend], NA)
    attr(colina, "ncurvas") <- length(rends)
    attr(colina, "rho") <- rho
    attr(colina, "g")   <- g

    return(colina)
}

# METODOS ------------------------------------------------------------------------------------------

#' Conversor Para \code{curvacolina}
#' 
#' Forca um objeto tipo \code{data.frame} para classe \code{curvacolina}
#' 
#' @param x \code{data.frame}-like a ser convertido
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

as.curvacolina <- function(x) {

    hl <- pot <- vaz <- rend <- NULL

    if(!("data.frame" %in% class(x))) stop("Argumento deve ser um data.frame ou data.table")

    if(!all(c("hl", "pot", "vaz", "rend") %in% colnames(x))) {
        stop("Verifique se as colunas 'hl', 'pot', 'vaz', 'rend' constam no dado")
    }

    x <- as.data.table(x)
    x <- x[, list(hl, pot, vaz, rend)]
    x <- list(CC = x)

    class(x) <- "curvacolina"
    attr(x, "rends") <- unique(x$CC$rend)
    attr(x, "ncurvas") <- length(attr(x, "rends"))

    return(x)
}

#' @export 

print.curvacolina <- function(x, ...) summary(x)

#' Sumario De \code{curvacolina}
#'
#' Produz um breve sumario da curva colina
#' 
#' @param object objeto \code{curvacolina}
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return imprime um sumario da curva colina, sem retornar nada
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

summary.curvacolina <- function(object, ...) {
    hl <- pot <- rend <- NULL
    cat("Numero de curvas:     ", attr(object, "ncurvas"), "\n")
    cat("Faixa de queda:       ", object$CC[, range(hl)], "\n")
    cat("Faixa de potencia:    ", object$CC[, range(pot)], "\n")
    cat("Faixa de rendimentos: ", range(attr(object, "rends")), "\n")
}

#' Escrita De \code{curvacolina}
#' 
#' Metodo para facilitacao de escrita de \code{curvacolina} lida pelas funcoes do pacote
#' 
#' @param x objeto \code{curvacolina} a ser escrito
#' @param file caminho para escrita. Deve possuir extensao -- caso nao possua, sera adicionada
#' 
#' @return Escreve \code{x} no caminho especificado como um csv de quatro colunas
#' 
#' @family curvacolina
#' 
#' @import data.table
#' 
#' @export

write.curvacolina <- function(x, file) fwrite(x$CC, file, quote = FALSE, sep = ";")

# HELPERS ------------------------------------------------------------------------------------------

#' Redutor De \code{curvacolina}
#' 
#' Retorna o objeto \code{curvacolina} com um numero reduzido de observacoes
#' 
#' O argumento \code{taxa} e utilizado da seguinte maneira: seja \code{n} o numero de observacoes em
#' \code{colina}, sera amostrado um subset dessas tomando uma a cada \code{taxa} observacoes. Por
#' exemplo, se \code{taxa = 3}, sao selecionadas as observacoes 1, 4, 7, e assim por diante.
#' 
#' O processo de selecao descrito acima e realizado \bold{por curva de rendimento}, dado que a curva
#' tenha pelo menos \code{5 * taxa} observacoes. Este limite e algo essencialmente arbitrario.
#' 
#' @param colina objeto \code{curvacolina} retornado pelas funcoes de leitura
#' @param taxa inteiro indicando reducao do tamanho do historico em \code{taxa_reducao} vezes. Ver 
#'     Detalhes
#' 
#' @return \code{colina} passada com observacoes selecionadas
#' 
#' @export

reduzcolina <- function(colina, taxa) {

    vec_reduz <- rep(FALSE, taxa)
    vec_reduz[1] <- TRUE

    colreduzida <- copy(colina$CC)
    colreduzida <- split(colreduzida, colreduzida$rend)
    colreduzida <- lapply(colreduzida, function(d) {
        if(nrow(d) > (5 * taxa)) {
            d <- d[rep(vec_reduz, length.out = .N)]
        }
        d
    })
    colreduzida <- as.curvacolina(rbindlist(colreduzida))

    return(colreduzida)
}