test_that("Visuazalicao de curvacolina", {
    colina2 <- learqprocit(system.file("extdata/procit_cc_alterada.xlsx", package = "colinapython"))

    p2d <- plot(colinadummy, "2d", print = FALSE)
    p3d <- plot(colinadummy, "3d", print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(colina2[[1]], "2d", print = FALSE, modo = "vaz")
    p3d <- plot(colina2[[1]], "3d", print = FALSE, modo = "vaz")

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))
})

test_that("Visuazalicao de Interpolador", {
    tt <- triangulacao(colinadummy)

    # RODADA SIMPLES
    p2d <- plot(tt, "2d", print = FALSE)
    p3d <- plot(tt, "3d", print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # testando argumentos de grade
    p2d <- plot(tt, "2d", print = FALSE, dhl = 20)
    p3d <- plot(tt, "3d", print = FALSE, dhl = 20)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(tt, "2d", print = FALSE, dpot = 20)
    p3d <- plot(tt, "3d", print = FALSE, dpot = 20)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(tt, "2d", print = FALSE, expande = c(.05, .05))
    p3d <- plot(tt, "3d", print = FALSE, expande = c(.05, .05))

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # add_colina = FALSE
    p2d <- plot(tt, "2d", add_colina = FALSE, print = FALSE)
    p3d <- plot(tt, "3d", add_colina = FALSE, print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # interpolador por vazao
    colina2 <- learqprocit(system.file("extdata/procit_cc_alterada.xlsx", package = "curvacolina"))
    tt <- triangulacao(colina2[[1]], modo = "vaz")

    p2d <- plot(tt, "2d", print = FALSE)
    p3d <- plot(tt, "3d", print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))
    tt <- triangulacao(colina2[[1]], modo = "vaz")

    p2d <- plot(tt, "2d", print = FALSE, dvaz = 50)
    p3d <- plot(tt, "3d", print = FALSE, dvaz = 50)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(tt, "2d", print = FALSE, byvaz = 10)
    p3d <- plot(tt, "3d", print = FALSE, byvaz = 10)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # add_colina = FALSE
    p2d <- plot(tt, "2d", add_colina = FALSE, print = FALSE)
    p3d <- plot(tt, "3d", add_colina = FALSE, print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))
})

test_that("Visuazalicao de gradecolina", {
    tt <- triangulacao(colinadummy)
    grade <- predict(tt, coordgrade(colinadummy, 20, 20), as.gradecolina = TRUE)

    # rodada simples
    p2d <- plot(grade, "2d", print = FALSE)
    p3d <- plot(grade, "3d", print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # add_colina = FALSE
    p2d <- plot(grade, "2d", add_colina = FALSE, print = FALSE)
    p3d <- plot(grade, "3d", add_colina = FALSE, print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # interpolador por vazao
    colina2 <- learqprocit(system.file("extdata/procit_cc_alterada.xlsx", package = "curvacolina"))
    tt <- triangulacao(colina2[[1]], modo = "vaz")
    grade <- predict(tt, coordgrade(colina2[[1]], 20, dvaz = 20), as.gradecolina = TRUE)

    p2d <- plot(grade, "2d", print = FALSE)
    p3d <- plot(grade, "3d", print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    # add_colina = FALSE
    p2d <- plot(grade, "2d", add_colina = FALSE, print = FALSE)
    p3d <- plot(grade, "3d", add_colina = FALSE, print = FALSE)

    expect_equal(class(p2d), c("gg", "ggplot"))
    expect_equal(class(p3d), c("plotly", "htmlwidget"))
})
