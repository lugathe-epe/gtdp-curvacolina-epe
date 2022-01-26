##################################### FUNCOES PARA INTERPOLACAO ####################################

#' Modelo Para Interpolacao De Curva Colina
#' 
#' Funcao para estimacao de diferentes interpoladores de curva colina
#' 
#' \code{interpolador} serve como uma interface comum para estimacao de diversas formas atraves das
#' quais interpolar a colina original. Atualmente ha tres abordagens implementadas:
#' 
#' \itemize{
#' \item{\code{"triangulacao"}}
#' \item{\code{"thinplate"}}
#' \item{\code{"tensorprod"}}
#' }
#' 
#' Se \code{metodo = "triangulacao"}, a colina fornecida e projetada no plano hl x pot e entao 
#' tesselada atraves da triangulacao de Delaunay. Interpolacao de um ponto em objetos de 
#' triangulacao se da atraves de transformacao para coordenadas baricentricas e subsequente media
#' ponderada dos rendimentos nos vertices do triangulo que o contem.
#' 
#' Quando \code{metodo = "thinplate"} ou \code{metodo = "tensorprod"}, a colina e suavizada atraves 
#' de splines, cujo tipo depende do \code{metodo} passado: \code{"thinplate"} corresponde a 
#' suavizacao por splines homonimas e \code{"tensorprod"} ao produto tensor de splines. A
#' interpolacao nesse caso consiste simplesmente da consulta a curva suavizada em pontos 
#' selecionados.
#' 
#' O argumento \code{...} so tem uso quando \code{metodo} corresponde a uma suavizacao por splines.
#' Nestes casos, \code{...} pode conter qualquer um dos argumentos passados ao construtor do modelo,
#' isto, e, tipo de spline (\code{bs}) e dimensao da base (\code{k}). Ver (\code{\link[mgcv]{te}}).
#' 
#' @param colina objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)
#' @param metodo um de \code{c("triangulacao", "thinplate", "tensorpro")}. Ver Detalhes
#' @param ... demais parametros que possam ser passados as funcoes de ajuste de cada \code{metodo}.
#'     Ver Detalhes
#' 
#' @examples
#' 
#' # usando dado dummy contido no pacote
#' interp_tri <- interpolador(colinadummy, "tri")
#' 
#' # interpolando uma grade 20x20 no dominio da colina
#' pontos <- geragrade(colinadummy, 20, 20)
#' interp <- predict(interp_tri, pontos)
#' 
#' \dontrun{
#' # visualizacao 3d e 2d do resultado
#' plot(interp)
#' plot(interp, "2d")
#' }
#' 
#' @return objeto da classe \code{interpolador} e subclasse \code{metodo}, isto e, um modelo com o 
#'     qual se realizar previsoes e, assim, interpolar o dado original
#' 
#' @family interpolador
#' 
#' @export

interpolador <- function(colina, metodo = c("triangulacao", "thinplate", "tensorprod"), ...) {

    metodo <- match.arg(metodo)

    interp_func <- match.call()
    interp_func[[1]] <- as.name(metodo)
    interp_func$metodo <- NULL

    interp <- eval(interp_func, envir = parent.frame())

    return(interp)
}

# METODOS ------------------------------------------------------------------------------------------

#' @rdname getcolina

getcolina.interpolador <- function(object) stop(paste0("Implemente metodo 'getcolina' do modelo: ", class(object)[1]))

#' Previsao Com Modelos Interpoladores
#' 
#' Metodo para interpolar curva colina a partir de um objeto gerado por \code{interpolador}
#' 
#' O argumento \code{as.gradecolina} permite alterar a saida da interpolacao. Se 
#' \code{as.gradecolina = FALSE}, o padrao, apenas o vetor de rendimentos interpolados sera
#' retornado. Caso \code{as.gradecolina = TRUE}, a saida sera um objeto \code{\link{gradecolina}}.
#' 
#' @param object objeto da classe \code{interpolador} retornado pela funcao homonima
#' @param pontos data.frame ou matriz contendo pontos nos quais amostrar o rendimento
#' @param as.gradecolina booleano indicando o nivel de detalhe na saida da funcao. Ver Detalhes
#' @param ... demais parametros que possam ser passados aos metodos de \code{predict} especificos
#' 
#' @return vetor de rendimentos nas coordenadas especificadas em \code{pontos}
#' 
#' @family interpolador
#' 
#' @export

predict.interpolador <- function(object, pontos, as.gradecolina, ...) {
    stop(paste0("Implemente metodo 'predict' do modelo: ", class(object)[1]))
}