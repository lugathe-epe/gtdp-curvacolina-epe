# curvacolina 1.9.2

## Bug fixes

* Corrige `orderpoly` que ainda nao estava preparada para interpoladores por vazao

# curvacolina 1.9.1

## New features

* Suporte a tratamento dos dados por vazao
  * `interpolador` agora recebe argumento `modo` indicando se a curva colina modelada e do tipo
    `hl x pot` ou `hl x vaz`
  * Funcoes de visualizacao agora suportam plots de colina e ajustes por vazao
  * Metodos de `gradecolina` adaptados para uso com grades geradas de interpoladores por vazao
  * `coordgrade` agora pode gerar discretizacao de dominios `hl x vazao`, alem de `hl x potencia`

## Bug fixes

* Funcoes construtoras de objeto `curvacolina` agora preservam colunas de vazao precalculadas, que 
  anteriormente acabavam sendo descartadas

# curvacolina 1.8.0

## New features

* Adiciona opcao de uso de dois tipos diferentes de interpoladores em uma mesma colina, fornecendo
  em `metodos` um vetor de duas posicoes, indicando os metodos, e o argumento `quebra`, indicando
  o valor de rendimento da curva a partir da qual usar o segundo tipo de interpolador
* Adicionada as funcoes `tessdelaunay` e `tessradial` para realizacao da tesselacao dos dados na
  interpolacao por triangulacao. `tessradial` implementa um algoritmo custom que garante o uso do 
  ponto maximo em todos os triangulos internos a curva de maior rendimento
* Conjuntamente com essas funcoes, `triangulacao` agora recebe o argumento `tessfunc`, atraves do
  qual pode ser informada qual funcao de tesselacao usar

### Minor

* Adiciona metodo `rbind.curvacolina`. Normalmente so tera uso interno em conjunto com os 
  interpoladores multiplos. Deve ser notado que o objeto retornado remove quaisquer linhas 
  duplicadas que possam resultar da combinacao
* Objetos `curvacolina` agora possuem um metodo de subset por expressoes de data.table
* Pequenas melhorias de documentacao

# curvacolina 1.6.2

## New features

### Minor

* adicionados testes para funcoes de visualizacao e retrocompat
* testes de interpolador mais robustos
* testes de curva colina agora cobrem toda a variacao de comportamento das funcoes

## Bug fixes

* correcao da identificacao de erro em `parsedadoscolina` quando `force = TRUE`. A coacao de colunas 
  para numerico dava um erro, como desejado, mas um erro nao controlado. O controle foi corrigido
  para entrar em acao corretamente
* correcao da identificacao de colina original nas leituras de `retamindist` era falha, so 
  funcionava em casos simples quando nao havia indice no arquivo de interpolacao antiga. 
* correcao de erro de execucao em `plot.gradecolina` quando `add_colina = FALSE`

# curvacolina 1.6.1

## Bug fixes

* Corrige erro rebuilding vignettes em mac-os. `rmarkdown` adicionado a lista de vignettebuilders

# curvacolina 1.6

## New features

* adicionada uma vignette ao pacote ("curvacolina")
* foi incluida a funcao `set_grho` que permite a definicao de aceleracao da gravidade e densidade
  da agua diretamente a um objeto `curvacolina`, sem precisar gerar o objeto mais uma vez
* adicionada funcao `write.gradecolina` para escrita das grades extraidas
* `as.curvacolina` agora esta mais robusta, passando por `new_curvacolina`. Tambem foram adicionados
  diversos checks sobre o dado sendo transformado e uma opcao `force` que permite ignorar os checks 
  e coagir o dado passado para o formato `curvacolina`. A funcao tambem possui os argumentos `g` e 
  `rho`

### Minor

* `geragrade` renomeada como `coordgrade` (#7)
* melhora o processamento de argumentos de grade passados para `plot.interpolador`
* `predict.gradecolina` agora retorna coluna `inhull` quando `full.output = TRUE`

## Bug fixes

* `predict.retamindist` adequada para usar argumento `as.gradecolina` (#6)
* `plot.gradecolina` continha um bud no plot de superfície que gerava um artefato nas quedas mais 
  baixas. Isto era devido ao fato de que `dcast` retém a primeira coluna como a variável RHS da 
  transformação para formato amplo. Esta coluna agora é removida para plotagem

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
