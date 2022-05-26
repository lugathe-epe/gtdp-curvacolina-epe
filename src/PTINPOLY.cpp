////////////////// FUNCAO PARA IDENTIFICAR SE UM PONTO ESTA DENTRO DE UM POLIGONO //////////////////

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <iostream>

// [[Rcpp::export]]
arma::Col<unsigned int> PTINPOLY(arma::mat& points, arma::mat& poly) {

    std::cout << "inicio" << "\n";

    unsigned int i, j, k;
    
    double x, y;

    bool inside = false;
    arma::Col<unsigned int> out(points.n_rows);

    std::cout << "check 1" << "\n";

    for(k = 0; k < points.n_rows; k++) {

        inside = false;

        x = points(k, 0), y = points(k, 1);

        std::cout << "check 2 --- " << k << "\n";

        for (i = 0, j = poly.n_rows - 1; i < poly.n_rows; j = i++) {

            double xi = poly(i,0), yi = poly(i,1);
            double xj = poly(j,0), yj = poly(j,1);
            
            // See if point is inside polygon
            inside ^= (((yi >= y) != (yj >= y)) && (x <= (xj - xi) * (y - yi) / (yj - yi) + xi));
        }
        out[i] = inside * 1;
    }
    
    // Is the cat alive or dead?
    return out;
}