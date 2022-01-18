# master (dev)

* `plot.interpolador` agora usa `...` para passar argumentos a `geragrade`, permitindo maior 
  controle sobre a superficie gerada

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
