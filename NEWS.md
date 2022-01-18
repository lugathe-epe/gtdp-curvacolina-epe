# gradecolina (dev)

* `gradecolina` agora aceita parametro de expansao. So e usado se `dhl` e `dpot` forem escalares

* `gradecolina` tem parametros `byhl` e `bypot`. Estes argumentos sao interpretados como o intervalo
de separacao entre cada divisao de queda e potencia na grade. Para manter a consistencia, a faixa de
quedas e potencias usada nessa construcao nao sao exatamente aquelas contidas na colina, como quando
gerando grades a partir de `dhl` e `dpot` (pois muito provavelmente uma sequencia
comecando em min(hl) andando de byhl em byhl nao terminaria em max(hl)). Os minimos e maximos das
faixas serao obtidos segundo as regras:

    * minimo: o maior multiplo de `byX` menor que min(X)
    * maximo: o menor multiplo de `byX` maior que max(X)

# curvacolina 1.0.1

## Bug fixes

* `thinplate` agora usa `fields::Tps` no lugar de `mgcv::gam` (#2). Aparentemente ocorre um problema
  de instabilidade numerica em funcao do tamanho da base e diferenca de escala durante o ajuste
  feito por `mgcv::gam`. Ao escalonar os regressores para o intervalo `[0, 1]`, os residuos voltam a
  ser efetivamente nulos. Ainda assim, foi observado que `fields::Tps` estima e preve muito mais
  rapido, alem de ja lidar com o escalonamento automaticamente. Foi optado por mover o backend de
  estimacao para ele

# curvacolina 1.0

Funcoes para extracao dos dados de curva colina digitalizada a partir de planilhas 
especificas ou daquelas de processos iterativos. Suavizacao e/ou interpolacao destes dados, 
visando a amostragem em grade de modo a converter a curva colina para uma representacao em 
tabela. Visualizacao tridimensional e bidimensional dos dados originais e sua modelagem.
