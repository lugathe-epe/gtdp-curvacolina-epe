##################################### FUNCOES PARA VISUALIZACAO ####################################

#' Visualizacao De \code{curvacolina}
#' 
#' Plots bi ou tridimensionais de curvas colinas
#' 
#' @param x objeto \code{curvacolina} retornado por uma das funcoes de leitura
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @examples 
#' 
#' arq <- system.file("extdata/colina.xlsx", package = "curvacolina")
#' colina <- learqcolina(arq)
#' 
#' \dontrun{
#' # plot 3d
#' plot(colina, "3d")
#' plot(colina, "2d")
#' 
#' # execucao silenciosa e posterior exibicao
#' p <- plot(colina, "2d", print = FALSE)
#' print(p)
#' }
#' 
#' @return se \code{tipo = "3d"} um objeto \code{plotly}, do contrario um objeto \code{ggplot} 
#'     contendo o plot. Em ambos os casos o grafico so sera exibido ao usuario caso 
#'     \code{print = TRUE} (o padrao).
#' 
#' @family curvacolina
#' 
#' @importFrom plotly plot_ly `%>%` layout hide_colorbar
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_d labs theme_bw guides guide_legend
#' 
#' @export 

plot.curvacolina <- function(x, tipo = c("3d", "2d"), print = TRUE, ...) {

    hl <- pot <- rend <- rend_label <- NULL

    tipo <- match.arg(tipo)

    dplot <- copy(x$CC)
    dplot[, rend_label := formatC(rend, format = "f", digits = 5, drop0trailing = TRUE)]

    if(tipo == "3d") {
        dplot[, rend_label := paste0("Rend = ", rend_label, "%")]

        p <- plot_ly(dplot, x = ~hl, y = ~pot, z = ~rend, color = ~rend_label, 
            colors = viridisLite::viridis(attr(x, "ncurvas")),
            type = "scatter3d", mode = "markers") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda L\U00EDquida (m)")),
                yaxis = list(title = list(text = "Pot\U00EAncia (MW)")),
                zaxis = list(title = list(text = "Rendimento (%)")))
            )

        if(print) print(p)

        invisible(p)
    } else {

        p <- ggplot(dplot, aes(hl, pot, color = rend_label)) + geom_point() +
            scale_color_viridis_d(name = "Rendimento (%)") +
            labs(x = "Queda L\U00EDquida (m)", y = "Pot\U00EAncia (MW)") +
            theme_bw() +
            guides(color = guide_legend(ncol = 1))

        if(print) print(p)

        invisible(p)
    }
}

#' Visualizacao De Interpolacoes
#' 
#' Funcao para visualizacao dos modelos ajustados atraves de \code{interpolador}
#' 
#' Para plotar uma superficie e necessario amostrar pontos numa grade regular. A definicao desta 
#' grade e realizada atraves do argumento \code{...}, que sera utilizado numa chamada de 
#' \code{\link{geragrade}} e esta grade amostrada do interpolador \code{x}. E possivel deixa-lo nao
#' especificado; neste caso sera usado o padrao \code{list(dhl = 200, dpot = 200)}, que deve ser
#' suficiente para um plot suave na maioria das circunstancias.
#' 
#' @param x objeto \code{interpolador} retornado pela funcao homonima
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param add_colina booleano indicando se os pontos da colina original tambem devem ser plotados
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
#' @param ... parametros passados para \code{\link{geragrade}} para amostragem da superficie. Se 
#'     deixado vazio e gerada uma grade 200 x 200. Ver Detalhes
#' 
#' @return Se \code{tipo = "3d"} um objeto \code{plotly} contendo o plot 3d; se \code{tipo = "2d"}
#'     um objeto \code{ggplot} contendo o plot 2d. Em ambos os casos o grafico so sera exibido ao
#'     usuario caso \code{print = TRUE} (o padrao).
#' 
#' @family interpolador
#' 
#' @import data.table
#' @importFrom plotly plot_ly `%>%` add_markers add_surface layout hide_colorbar hide_legend
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_d labs theme_bw guides guide_legend
#'     geom_raster scale_fill_viridis_c
#' 
#' @export 

plot.interpolador <- function(x, tipo = c("3d", "2d"), add_colina = TRUE, print = TRUE, ...) {

    hl <- pot <- rend <- NULL

    tipo <- match.arg(tipo)

    ggargs <- list(...)
    if(length(ggargs) == 0) ggargs <- list(dhl = 200, dpot = 200)

    if(add_colina) colina <- copy(getcolina(x)$CC) else colina <- data.table(hl = NA, pot = NA, rend = 0)

    dsurf <- do.call(geragrade, c(list(colina = getcolina(x)), ggargs))
    dsurf <- predict(x, dsurf, TRUE)

    if(tipo == "3d") {
        p <- plot_ly() %>%
            add_markers(x = colina$hl, y = colina$pot, z = colina$rend,
                type = "scatter3d", name = "colina") %>%
            add_surface(x = unique(dsurf$hl), y = unique(dsurf$pot),
                z = t(data.matrix(dcast(dsurf, hl ~ pot, value.var = "rend"))),
                name = "interpolacao") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda L\U00EDquida (m)")),
                yaxis = list(title = list(text = "Pot\U00EAncia (MW)")),
                zaxis = list(title = list(text = "Rendimento (%)")))
            )

        if(print) print(p)

        invisible(p)
    } else {
        colina[, rend := factor(paste0(formatC(rend, format = "f", digits = 3), "%"))]
        dsurf <- dsurf[complete.cases(dsurf)]
        p <- ggplot() +
            geom_raster(data = dsurf, aes(hl, pot, fill = rend)) +
            geom_point(data = colina, aes(hl, pot), color = "blue") +
            scale_fill_viridis_c(name = "Rendimento (%)", na.value = NA) +
            labs(x = "Queda L\U00EDquida (m)", y = "Pot\U00EAncia (MW)") +
            theme_bw() +
            guides(color = guide_legend(ncol = 1))

        if(print) print(p)

        invisible(p)
    }
}