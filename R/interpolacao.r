##################################### FUNCOES PARA INTERPOLACAO ####################################

predict <- function(...) {
  stats::predict(...)
}

#' Modelo Para Interpolacao De Curva Colina
#' 
#' Funcao para estimacao de diferentes interpoladores de curva colina
#' 
#' \code{interpolador} serve como uma interface comum para estimacao de diversas formas atraves das
#' quais interpolar a colina original. Atualmente ha tres abordagens implementadas:
#' 
#' \itemize{
#' \item{\code{\link{triangulacao}}}
#' \item{\code{\link{thinplate}}}
#' \item{\code{\link{tensorprod}}}
#' }
#' 
#' Se \code{metodo = "triangulacao"}, a colina fornecida e projetada no plano hl x pot/vaz e entao 
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
#' O argumento \code{...} permite passar os argumentos opcionais aos metodos especificos. As paginas
#' de ajuda dos interpoladores proveem mais informacao a respeito destes argumentos extras.
#' 
#' \bold{Interpoladores Multiplos}:
#' 
#' Existe a possibilidade de estimar um interpolador combinando mais de um metodo. Isto pode ser
#' feito passando um vetor de duas posicoes em \code{metodo} indicando os dois metodos a serem
#' usados conjuntamente com o argumento \code{quebra}, um numero indicando o rendimento da curva
#' a partir da qual o segundo metodo deve ser utilizado.
#' 
#' Nestes casos, o objeto retornado tera classe \code{"interpolador"} e subclasse 
#' \code{"interpoladorM"}, com todos os mesmos metodos disponivies.
#' 
#' @param colina objeto da classe \code{curvacolina} (retornado pelas funcoes de leitura)
#' @param metodo um ou dois de \code{c("triangulacao", "thinplate", "tensorpro")}. Ver Detalhes
#' @param quebra opcional, numerico indicando o rendimento da curva a partir da qual chavear
#'     metodos. Ver Detalhes
#' @param modo um de \code{c("pot", "vaz")}, indicando qual o modo de curva colina esta sendo
#'     modelada
#' @param ... demais parametros que possam ser passados as funcoes de ajuste de cada \code{metodo}.
#'     Ver Detalhes
#' 
#' @examples
#' 
#' # usando dado dummy contido no pacote
#' interp_tri <- interpolador(colinadummy, "triangulacao")
#' 
#' # interpolando uma grade 20x20 no dominio da colina
#' pontos <- coordgrade(colinadummy, 20, 20)
#' grade <- predict(interp_tri, pontos)
#' 
#' \dontrun{
#' # visualizacao 3d e 2d do resultado
#' plot(grade)
#' plot(grade, "2d")
#' }
#' 
#' # INTERPOLADOR MULTIPLO
#' interp_mult <- interpolador(colinadummy[rend > 92], c("thinplate", "triangulacao"), 95.5)
#' 
#' # interpolando uma grade 20x20 no dominio da colina
#' pontos <- coordgrade(colinadummy, 20, 20)
#' grade <- predict(interp_mult, pontos)
#' 
#' \dontrun{
#' # visualizacao 3d e 2d do resultado
#' plot(grade)
#' plot(grade, "2d")
#' }
#' 
#' @return objeto da classe \code{interpolador} e subclasse \code{metodo}, isto e, um modelo com o 
#'     qual se realizar previsoes e, assim, interpolar o dado original
#' 
#' @family interpolador
#' 
#' @export

interpolador <- function(colina, metodo, quebra, modo = "pot", ...) {

    rend <- NULL

    if(missing(quebra)) {
        interp_func <- match.call()
        interp_func[[1]] <- as.name(metodo)
        interp_func$metodo <- NULL
        interp_func$modo <- modo

        interp <- eval(interp_func, envir = parent.frame())

        return(interp)
    } else {

        if(length(metodo) != 2) stop("Quando 'quebra' e fornecido, 'metodos' deve ter tamanho 2")

        colinas <- list(colina[rend <= quebra], colina[rend >= quebra])

        interp_func <- match.call()
        interp <- mapply(metodo, colinas, FUN = function(m, c) {
            interp_func[[1]] <- as.name(m)
            interp_func$colina <- c
            interp_func$metodo <- NULL
            interp_func$modo <- modo

            eval(interp_func, envir = parent.frame())
        }, SIMPLIFY = FALSE)

        new_interpoladorM(interp, quebra, modo)
    }
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
