# dev (master)

## New features

* `as.curvacolina` agora esta mais robusta, passando por `new_curvacolina`. Tambem foram adicionados
  diversos checks sobre o dado sendo transformado e uma opcao `force` que permite ignorar os checks 
  e coagir o dado passado para o formato `curvacolina`. A funcao tambem possui os argumentos `g` e 
  `rho` 

* foi incluida a funcao `set_grho` que permite a definicao de aceleracao da gravidade e densidade
  da agua diretamente a um objeto `curvacolina`, sem precisar gerar o objeto mais uma vez

# curvacolina 1.5

## New features

* objetos `curvacolina` agora carregam attr `g`, `rho` e `max`; `colinadummy` atualizada para o novo
  padrao
* introducao de objetos da classe `gradecolina`.
* metodo de plot para `gradecolina`
* argumento `full.output` de `predict` passou a ser `as.curvacolina`, e agora o que se retorna 
quando este argumento e verdadeiro e um objeto `gradecolina`
* `interpolagrade` foi reformulado como metodos da classe `gradecolina`

    * `interpolagrade.curvacolina` -> `fitted.gradecolina`
    * `interpolagrade.data.frame` -> `predict.gradecolina`

A ordem e nome dos argumentos passados tambem foi invertida para consistencia com os metodos S3 da 
linguagem -- consulte a pagina de ajuda para mais detalhes `?interpolacao_bilin`

### Minor

* homogeinizacao das legendas e parametros graficos gerais nas funcoes de `plot`, tando em 2d quanto
  em 3d
* plot de `interpolador` agora e so um wrapper em torno do de `gradecolina`

## Bug fixes

* corrige erro em `interpolagrade`. A implementacao original reordenava os pontos para entrar no
  interpolador de grade em cpp, mas nao retornava a ordem original depois, gerando um erro.

# curvacolina 1.3

## New features

* `gradecolina` agora aceita parametro de expansao. So e usado se `dhl` e `dpot` forem escalares

* `gradecolina` tem parametros `byhl` e `bypot`. Estes argumentos sao interpretados como o intervalo
de separacao entre cada divisao de queda e potencia na grade. Para manter a consistencia, a faixa de
quedas e potencias usada nessa construcao nao sao exatamente aquelas contidas na colina, como quando
gerando grades a partir de `dhl` e `dpot` (pois muito provavelmente uma sequencia
comecando em min(hl) andando de byhl em byhl nao terminaria em max(hl)). Os minimos e maximos das
faixas serao obtidos segundo as regras:

    * minimo: o maior multiplo de `byX` menor que min(X)
    * maximo: o menor multiplo de `byX` maior que max(X)

* adiciona generica `interpolagrade`, com metodos para `data.frame(table)` e `curvacolina`. Esta
  funcao facilita o processo de interpolacao bilinear de pontos arbitrarios numa grade regular

* `plot.interpolador` agora usa `...` para passar argumentos a `geragrade`, permitindo maior 
  controle sobre a superficie gerada

* classe `retamindist` incluida. Proporciona retrocompatibilidade com arquivos de colina interpolada
  gerados a partir da interpolacao por reta de menor distancia implementada na macro

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
