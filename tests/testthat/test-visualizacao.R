test_that("Visuazalicao de curvacolina", {
    p2d <- plot(colinadummy, "2d", print = FALSE)
    p3d <- plot(colinadummy, "3d", print = FALSE)

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
})
