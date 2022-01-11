####################################### FUNCOES PARA LEITURA #######################################

#' Le Planilha de Curva Colina
#' 
#' Funcoes para leitura de arquivos xlsx e extracao da curva colina em formato padronizado
#' 
#' \code{learqcolina} deve ser utilizada para leitura de arquivos especificos de curva colina, nos 
#' quais existe apenas uma aba contendo a informacao das curvas. \code{learqprocit} e especifica 
#' para uso com planilhas de processo iterativo, nas quais pode haver mais de uma curva colina.
#' 
#' @param arq caminho da planilha
#' @param aba aba a ser lida. Por padrao igual a \code{1}, geralmente nao deve ser informado pelo 
#'     usuario
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
#' \item{\code{rend}}{rendimento correspondente}
#' }
#' 
#' \code{leaqrcolina} retorna apenas um objeto, enquanto \code{learqprocit} retornara uma lista de
#' tantos elementos quanto abas de curva colina existem na planilha
#' 
#' @rdname leexcel
#' 
#' @family curvacolina
#' 
#' @export

learqcolina <- function(arq, aba = 1) {

    plan <- as.data.frame(readxl::read_xlsx(arq, aba, col_names = FALSE, .name_repair = "minimal"))

    rends <- unname(unlist(plan[1, ]))
    rends <- rends[!is.na(rends)]
    rends <- as.numeric(regmatches(rends, regexpr("[[:digit:]]+(\\.[[:digit:]]+)?", rends)))

    ncurvas <- (ncol(plan) + 1) / 3
    curvas  <- lapply(seq(ncurvas), function(i) {
        col1 <- 1 + (i - 1) * 3
        col2 <- col1 + 1
        out <- plan[, col1:col2]
        out <- out[complete.cases(out), ]
        out[] <- lapply(out, as.numeric)
        colnames(out) <- c("hl", "pot")
        out
    })

    new_curvacolina(rends, curvas)
}

#' @rdname leexcel
#' 
#' @export

learqprocit <- function(arq) {

    abas <- readxl::excel_sheets(arq)
    abas <- abas[grep("Colina", abas)]

    alterada <- grepl("Alterada", abas)
    if(any(alterada)) abas <- abas[alterada]

    out <- lapply(abas, function(a) learqcolina(arq, aba = a))

    if(length(out) > 1) names(out) <- paste0("colina_", seq(abas)) else out <- out[[1]]

    return(out)
}

#' @import data.table

new_curvacolina <- function(rends, curvas) {

    colina <- mapply(rends, curvas, FUN = function(r, c) cbind(c, rend = r), SIMPLIFY = FALSE)
    colina <- do.call(rbind, colina)
    colina <- as.data.table(colina)

    colina <- list(CC = colina)

    class(colina) <- c("curvacolina")
    attr(colina, "rends") <- rends
    attr(colina, "ncurvas") <- length(rends)

    return(colina)
}

# METODOS ------------------------------------------------------------------------------------------

#' @import data.table
#' 
#' @export

as.curvacolina <- function(x, ...) {

    if(!("data.frame" %in% class(x))) stop("Argumento deve ser um data.frame ou data.table")

    if(!all(c("hl", "pot", "vaz", "rend") %in% colnames(x))) {
        stop("Verifique se as colunas 'hl', 'pot', 'vaz', 'rend' constam no dado")
    }

    x <- as.data.table(x)
    x <- x[, .(hl, pot, vaz, rend)]
    x <- list(CC = x)

    class(x) <- "curvacolina"
    attr(x, "rends") <- unique(x$CC$rend)
    attr(x, "ncurvas") <- length(attr(x, "rends"))

    return(x)
}

#' @export 

print.curvacolina <- function(x, ...) summary(x)

#' @import data.table
#' 
#' @export

summary.curvacolina <- function(x, ...) {
    cat("Numero de curvas:     ", attr(x, "ncurvas"), "\n")
    cat("Faixa de queda:       ", x$CC[, range(hl)], "\n")
    cat("Faixa de potencia:    ", x$CC[, range(pot)], "\n")
    cat("Faixa de rendimentos: ", range(attr(x, "rends")), "\n")
}