////////////////// FUNCAO PARA IDENTIFICAR SE UM PONTO ESTA DENTRO DE UM POLIGONO //////////////////

#include <RcppArmadillo.h>

//' Implementacao Do Algoritmo Raycasting
//' 
//' Identifica quais observacoes em \code{pontos} estao dentro do poligono \code{poly}
//' 
//' Esta funcao assume que linhas de \code{poly} representam vertices adjacentes. Nao importa onde
//' o primeiro vertice esta, contanto que cada nova linha indique o proximo vertice do poligono em
//' um mesmo sentido de deslocamento. Sera considerado que o primeiro vertice e o adjacente ao 
//' ultimo no sentido de deslocamento.
//' 
//' @param points matriz contendo as coordenadas dos pontos a testar
//' @param poly matriz contendo as coordenadas dos vertices do poligono. Ver Detalhes
//' 
//' @return vetor coluna de 0s e 1s indicando quais pontos estao dentro do poligono
// [[Rcpp::export]]
arma::uvec PTINPOLY(arma::mat& points, arma::mat& poly) {

    arma::uvec out(points.n_rows, arma::fill::zeros);

    unsigned int i, j, k;
    for (i = 0, j = poly.n_rows - 1; i < poly.n_rows; j = i++) {

        double xi = poly(i,0), yi = poly(i,1);
        double xj = poly(j,0), yj = poly(j,1);

        double b_1 = (xj - xi) / (yj - yi);
        double b_0 = xi - yi * b_1;

        for(k = 0; k < points.n_rows; k++) {

            double x = points(k, 0), y = points(k, 1);

            out[k] ^= (((yi >= y) != (yj >= y)) && (x <= b_0 + b_1 * y));
        }
    }

    return out;
}
