##################################### FUNCOES PARA VISUALIZACAO ####################################

#' Visualizacao De \code{curvacolina}
#' 
#' Plots bi ou tridimensionais de curvas colinas
#' 
#' @param x objeto \code{curvacolina} retornado por uma das funcoes de leitura
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
#' @param modo um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
#'     modelada
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @examples 
#' 
#' arq <- system.file("extdata/procit_cc_alterada.xlsx", package = "curvacolina")
#' colina <- learqprocit(arq)
#' 
#' \dontrun{
#' # plot 3d
#' plot(colina[[1]], "3d")
#' plot(colina[[1]], "2d")
#' 
#' # execucao silenciosa e posterior exibicao
#' p <- plot(colina, "2d", print = FALSE)
#' print(p)
#' 
#' # plotando por vazao
#' plot(colina[[1]], "3d", modo = "vaz")
#' plot(colina[[1]], "2d", modo = "vaz")
#' 
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

plot.curvacolina <- function(x, tipo = c("3d", "2d"), print = TRUE, modo = "pot", ...) {

    hl <- Y <- rend <- rend_label <- NULL

    tipo <- match.arg(tipo)

    dplot <- copy(x$CC)
    dplot[, rend_label := formatC(rend, format = "f", digits = 5, drop0trailing = TRUE)]

    if(tipo == "3d") {
        dplot[, rend_label := paste0("Rend = ", rend_label, "%")]
        dplot[, Y := dplot[[modo]]]
        leg <- ifelse(modo == "pot", "Pot\U00EAncia (MW)", "Vaz\u00e3o Turbinada (m\u00b3/s)")

        p <- plot_ly(dplot, x = ~hl, y = ~Y, z = ~rend, color = ~rend_label,
            colors = viridisLite::viridis(attr(x, "ncurvas")),
            type = "scatter3d", mode = "markers") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda L\U00EDquida (m)")),
                yaxis = list(title = list(text = leg)),
                zaxis = list(title = list(text = "Rendimento (%)")))
            )

        if(print) print(p)

        invisible(p)
    } else {

        dplot[, Y := dplot[[modo]]]
        leg <- ifelse(modo == "pot", "Pot\U00EAncia (MW)", "Vaz\u00e3o Turbinada (m\u00b3/s)")

        p <- ggplot(dplot, aes(hl, Y, color = rend_label)) + geom_point() +
            scale_color_viridis_d(name = "Rendimento (%)") +
            labs(x = "Queda L\U00EDquida (m)", y = leg) +
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
#' \code{\link{coordgrade}} e esta grade amostrada do interpolador \code{x}. E possivel deixa-lo nao
#' especificado; neste caso sera usado o padrao de duzentas divisoes em cada eixo, que deve ser
#' suficiente para um plot suave na maioria das circunstancias.
#' 
#' @param x objeto \code{interpolador} retornado pela funcao homonima
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param add_colina booleano indicando se os pontos da colina original tambem devem ser plotados
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
#' @param ... parametros passados para \code{\link{coordgrade}} para amostragem da superficie. Se 
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
    modo <- attr(x, "modo")

    coord_args  <- list(...)

    minargs <- list(c("dhl", "byhl"), paste0(c("d", "by"), modo))
    tem_minargs <- sapply(minargs, function(x) any(x %in% names(coord_args)))

    if(!tem_minargs[1]) coord_args <- c(coord_args, list(dhl = 200))
    if(!tem_minargs[2]) coord_args <- c(coord_args, structure(list(200), names = paste0("d", modo)))

    dsurf <- do.call(coordgrade, c(list(colina = getcolina(x)), coord_args))
    dsurf <- predict(x, dsurf, TRUE)

    plot.gradecolina(dsurf, tipo, add_colina, print, modo)
}

#' Visualizacao De \code{gradecolina}
#' 
#' Funcao para visualizacao das grades regulares extraidas de interpoladores
#' 
#' @param x objeto \code{gradecolina} retornado pela funcao homonima
#' @param tipo um de \code{c("3d", "2d")} indicando o tipo de grafico desejado
#' @param add_colina booleano indicando se os pontos da colina original tambem devem ser plotados
#' @param print booleano indicando se o plot deve ser exibido. Caso \code{print = FALSE} o objeto
#'     sera retornado silenciosamente
#' @param ... existe somente para consistencia de metodos. Nao possui utilidade
#' 
#' @return Se \code{tipo = "3d"} um objeto \code{plotly} contendo o plot 3d; se \code{tipo = "2d"}
#'     um objeto \code{ggplot} contendo o plot 2d. Em ambos os casos o grafico so sera exibido ao
#'     usuario caso \code{print = TRUE} (o padrao).
#' 
#' @family gradecolina
#' 
#' @import data.table
#' @importFrom plotly plot_ly `%>%` add_markers add_surface layout hide_colorbar hide_legend
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_d labs theme_bw guides guide_legend
#'     geom_raster scale_fill_viridis_c
#' 
#' @export 

plot.gradecolina <- function(x, tipo = c("3d", "2d"), add_colina = TRUE, print = TRUE, ...) {

    hl <- Y <- rend <- NULL

    tipo <- match.arg(tipo)
    modo <- attr(x, "modo")

    if(add_colina) {
        colina <- copy(x$colina$CC)
    } else {
        colina <- data.table(hl = NA_real_, pot = NA_real_, vaz = NA_real_, rend = 0)
    }

    grade <- copy(x$grade)
    grade[, Y := grade[[modo]]]
    colina[, Y := colina[[modo]]]
    leg <- ifelse(modo == "pot", "Pot\U00EAncia (MW)", "Vaz\u00e3o Turbinada (m\u00b3/s)")

    if(tipo == "3d") {
        p <- plot_ly() %>%
            add_markers(x = colina$hl, y = colina$Y, z = colina$rend,
                type = "scatter3d", name = "colina") %>%
            add_surface(x = unique(grade$hl), y = unique(grade$Y),
                z = t(data.matrix(dcast(grade, hl ~ Y, value.var = "rend"))[, -1]),
                name = "interpolacao") %>%
            layout(scene = list(
                xaxis = list(title = list(text = "Queda L\U00EDquida (m)")),
                yaxis = list(title = list(text = leg)),
                zaxis = list(title = list(text = "Rendimento (%)")))
            )

        if(print) print(p)

        invisible(p)
    } else {
        colina[, rend := factor(paste0(formatC(rend, format = "f", digits = 3), "%"))]
        grade <- grade[complete.cases(grade)]
        p <- ggplot() +
            geom_raster(data = grade, aes(hl, Y, fill = rend)) +
            geom_point(data = colina, aes(hl, Y), color = "blue") +
            scale_fill_viridis_c(name = "Rendimento (%)", na.value = NA) +
            labs(x = "Queda L\U00EDquida (m)", y = leg) +
            theme_bw() +
            guides(color = guide_legend(ncol = 1))

        if(print) print(p)

        invisible(p)
    }
}