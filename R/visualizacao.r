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
#' @return um objeto \code{plotly} contendo o plot. Em ambos os casos o grafico so sera exibido ao
#'     usuario caso \code{print = TRUE} (o padrao).
#' 
#' @family curvacolina
#' 
#' @importFrom plotly plot_ly `%>%` layout hide_colorbar
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_d labs theme_bw guides guide_legend
#' 
#' @export 

plot.curvacolina <- function(x, tipo = c("3d", "2d"), print = TRUE, ...) {

    rend <- rend_label <- NULL

    tipo <- match.arg(tipo)

    dplot <- copy(x$CC)
    dplot[, rend_label := formatC(rend, format = "f", digits = 5, drop0trailing = TRUE)]

    if(tipo == "3d") {
        dplot[, rend_label := paste0("Rend = ", rend_label, "%")]

        p <- plot_ly(dplot, x = ~hl, y = ~pot, z = ~rend, color = ~rend_label, 
            colors = viridisLite::viridis(attr(x, "ncurvas")),
            type = "scatter3d", mode = "markers") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda Liquida")),
                yaxis = list(title = list(text = "Potencia")),
                zaxis = list(title = list(text = "Rendimento")))
            )

        if(print) print(p)

        invisible(p)
    } else {
        dplot[, rend_label := paste0(rend_label, "%")]

        p <- plot_ly(dplot, x = ~hl, y = ~pot, color = ~rend_label, 
            colors = viridisLite::viridis(attr(x, "ncurvas")),
            type = "scatter", mode = "markers") %>%
            layout(
                xaxis = list(title = list(text = "Queda Liquida")),
                yaxis = list(title = list(text = "Potencia"))
            )

        #ggplot(dplot, aes(hl, pot, color = rend_label)) + geom_point() +
            #scale_color_viridis_d(name = "Rendimento") +
            #labs(x = "Queda Liquida", y = "Potencia") +
            #theme_bw() +
            #guides(color = guide_legend(ncol = 1))

        if(print) print(p)

        invisible(p)
    }
}

#' Visualizacao De Interpolacoes
#' 
#' Funcao para visualizacao dos modelos ajustados atraves de \code{interpolador}
#' 
#' Os argumentos \code{hl} e \code{pot} devem ser fornecidos como um numero, indicando quantos 
#' cortes no eixo de queda e potencia, respectivamente, devem ser utilizados na geracao de uma grade 
#' a qual interpolar na superficie suavizada. Estes pontos serao entao utilizados para plot.
#' 
#' @param x objeto \code{interpolador} retornado pela funcao homonima
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param add_colina booleano indicando se os pontos da colina original tambem devem ser plotados
#' @param hl,pot vetores definindo grade amostrada. Ver Detalhes
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
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

plot.interpolador <- function(x, tipo = c("3d", "2d"), add_colina = TRUE, hl = 200, pot = 200,
    print = TRUE, ...) {

    rend <- NULL

    tipo <- match.arg(tipo)

    if(add_colina) colina <- copy(getcolina(x)$CC) else colina <- data.table(hl = NA, pot = NA, rend = 0)

    dsurf <- geragrade(getcolina(x), hl, pot)
    dsurf[, rend := predict(x, dsurf)]

    if(tipo == "3d") {
        p <- plot_ly() %>%
            add_markers(x = colina$hl, y = colina$pot, z = colina$rend,
                type = "scatter3d", name = "colina") %>%
            add_surface(x = unique(dsurf$hl), y = unique(dsurf$pot),
                z = t(data.matrix(dcast(dsurf, hl ~ pot, value.var = "rend"))),
                name = "interpolacao") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda Liquida")),
                yaxis = list(title = list(text = "Potencia")),
                zaxis = list(title = list(text = "Rendimento")))
            ) %>%
            hide_legend() %>%
            hide_colorbar()

        if(print) print(p)

        invisible(p)
    } else {
        colina[, rend := factor(paste0(formatC(rend, format = "f", digits = 3), "%"))]
        dsurf <- dsurf[complete.cases(dsurf)]
        p <- ggplot() +
            geom_raster(data = dsurf, aes(hl, pot, fill = rend)) +
            geom_point(data = colina, aes(hl, pot), color = "blue") +
            scale_fill_viridis_c(name = "Rendimento", na.value = NA) +
            labs(x = "Queda Liquida", y = "Potencia") +
            theme_bw() +
            guides(color = guide_legend(ncol = 1))

        if(print) print(p)

        invisible(p)
    }
}