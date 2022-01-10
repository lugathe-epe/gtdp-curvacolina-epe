##################################### FUNCOES PARA VISUALIZACAO ####################################

#' Visualizacao De \code{curvacolina}
#' 
#' Plots bi ou tridimensionais de curvas colinas
#' 
#' @param x objeto \code{curvacolina} retornado por uma das funcoes de leitura
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
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
#' @return Se \code{tipo = "3d"} um objeto \code{plotly} contendo o plot 3d; se \code{tipo = "2d"}
#'     um objeto \code{ggplot} contendo o plot 2d. Em ambos os casos o grafico so sera exibido ao
#'     usuario caso \code{print = TRUE} (o padrao).
#' 
#' @family curvacolina
#' 
#' @importFrom plotly plot_ly `%>%` layout hide_colorbar
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_d labs theme_bw guides guide_legend
#' 
#' @export 

plot.curvacolina <- function(x, tipo = c("3d", "2d"), print = TRUE, ...) {

    tipo <- match.arg(tipo)

    if(tipo == "3d") {
        p <- plot_ly(x$CC, x = ~hl, y = ~pot, z = ~rend, color = ~rend,
            type = "scatter3d", mode = "markers") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda Liquida")),
                yaxis = list(title = list(text = "Potencia")),
                zaxis = list(title = list(text = "Rendimento")))
            ) %>%
            hide_colorbar()

        if(print) print(p)

        invisible(p)
    } else {

        dplot <- copy(x$CC)
        dplot[, rend := factor(paste0(formatC(rend, format = "f", digits = 3), "%"))]
        p <- ggplot(dplot, aes(hl, pot, color = rend)) + geom_point() +
            scale_color_viridis_d(name = "Rendimento") +
            labs(x = "Queda Liquida", y = "Potencia") +
            theme_bw() +
            guides(color = guide_legend(ncol = 1))

        if(print) print(p)

        invisible(p)
    }
}

plot.interpolador <- function(x, hl, pot, print = TRUE, ...) {
    NA
}