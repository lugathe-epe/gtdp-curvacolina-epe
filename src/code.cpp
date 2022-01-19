////////////////////////////////////// FUNCOES DE INTERPOLACAO /////////////////////////////////////

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

//' Interpolacao Bilinear
//' 
//' Realiza interpolacao bilinear em uma grade tridimensional de valores. Funcao interna
//' 
//' Os argumentos \code{hlGrade} e \code{potGrade} devem corresponder as coordenadas de queda e 
//' potencia contidas na grade em ordem crescente. \code{rendGrade} deve ser uma matrix M x N
//' com o formato
//' 
//' |      | pot_1 | pot_2 |  ... | pot_N |
//' | ---- | ----- | ----- | ---- | ----- | 
//' | hl_1 | rend_11 | rend_12 | ... | rend_1N |
//' | hl_2 | rend_21 | rend_22 | ... | rend_2N |
//' | ...  | ...     | ...     | ... | ...     |
//' | hl_M | rend_M1 | rend_M2 | ... | rend_MN |
//' 
//' \code{hlPred} e \code{potPred} devem ser informados como vetores numericos indicando as 
//' coordenadas onde interpolar. Os pontos devem ser fornecidos ordenados por \code{potPred} em
//' ordem crescente. Isto e necessario para que se possa usar uma pesquisa mais otimizada na matriz
//' de rendimentos e, assim, tornar a funcao mais eficiente
//' 
//' @param hlGrade,potGrade,rendGrade valores das variaveis explicativas (x e y) e dependente (z), na grade
//' @param hlPred,potPred valores das variaveis explicativas a interpolar na grade
//' 
//' @return vetor de valores interpolados
// [[Rcpp::export]]
arma::vec INTERPBILIN( arma::vec &hlGrade, arma::vec &potGrade, arma::mat &rendGrade, 
    arma::vec &hlPred, arma::vec &potPred )
{
    
    // Extrai tamanho dos vetores de previsao
    int size = hlPred.size();
	
    // Inicializa variavel de saida
	arma::vec out(size);
	
    // Inicializa indice da vazao mais baixa
    int jY = 0;

    // Interpola para cada elemento do vetor de previsao
	for(int i = 0; i < size; ++i) {

        // Determina o indice da vazao mais baixa
		while(potGrade[jY + 1] < potPred[i]) jY++;

        // Inicializa indice da queda mais baixa
        int jX = 0;

        // Determina infice da queda mais baixa
        while (hlGrade[jX + 1] < hlPred[i]) jX++;

        // Identifica as coordenadas X dos pontos entre os quais se interpola
		double x1 = hlGrade[jX], x2 = hlGrade[jX + 1];
        
        // Identifica as coordenadas Y dos pontos entre os quais se interpola
		double y1 = potGrade[jY], y2 = potGrade[jY + 1];

        // Identifica as coordenadas Z dos pontos entre os quais se interpola
        double z11 = rendGrade(jX, jY), z12 = rendGrade(jX, jY+1), z21 = rendGrade(jX+1, jY), z22 = rendGrade(jX+1,jY+1);

        // Calcula as primeiras interpolacoes em X
        double f1 = (x2 - hlPred[i]) / (x2 - x1) * z11 + (hlPred[i] - x1) / (x2 - x1) * z21;
        double f2 = (x2 - hlPred[i]) / (x2 - x1) * z12 + (hlPred[i] - x1) / (x2 - x1) * z22;

        // Interpola em Y e preenche o vetor de saida
        out[i] = (y2 - potPred[i]) / (y2 - y1) * f1 + (potPred[i] - y1) / (y2 - y1) * f2;
		
	}

    // Retorna saida
	return out;
}