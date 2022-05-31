test_that("Interpolador sem subclasse", {
    interp <- structure(NA, class = "interpolador")

    expect_error(getcolina.interpolador(interp))
    expect_error(predict(interp, 1))
})

test_that("Modelagem por Triangulacao", {
    interp <- interpolador(colinadummy, "triangulacao")

    expect_equal(class(interp), c("triangulacao", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("triangulos", "colina"))

    expect_equal(getcolina.triangulacao(interp), colinadummy)

    # PREDICT VAZIO
    gg <- expand.grid(hl = 1:10, pot = NA)
    expect_equal(predict(interp, gg), numeric(0))

    gg <- coordgrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), "gradecolina")
    expect_equal(class(pred_full[[2]]), "curvacolina")
    expect_equal(colnames(pred_full[[1]]), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full[[1]]$rend, pred_vec)

    p3d <- plot(interp, print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, tipo = "2d", print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p2d), c("gg", "ggplot"))
})

test_that("Modelagem por Tensor Product", {

    # PASSANDO ARGUMENTOS EXTRAS

    interp <- interpolador(colinadummy, "tensorprod", k = 10)
    expect_equal(interp$superficie$rank, 100)

    interp <- interpolador(colinadummy, "tensorprod", bs = "cr")
    expect_equal(sapply(interp$superficie$smooth[[1]]$margin, class), rep("cr.smooth", 2))

    interp <- interpolador(colinadummy, "tensorprod")

    expect_equal(class(interp), c("tensorprod", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.tensorprod(interp), colinadummy)

    # PREDICT VAZIO
    gg <- expand.grid(hl = 1:10, pot = NA)
    expect_equal(predict(interp, gg), numeric(0))

    gg <- coordgrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), "gradecolina")
    expect_equal(class(pred_full[[2]]), "curvacolina")
    expect_equal(colnames(pred_full[[1]]), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full[[1]]$rend, pred_vec)

    p3d <- plot(interp, print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, tipo = "2d", print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p2d), c("gg", "ggplot"))
})

test_that("Modelagem por Thin Plate", {
    interp <- interpolador(colinadummy, "thinplate", taxa_reducao = 5)

    expect_equal(class(interp), c("thinplate", "interpolador"))
    expect_equal(length(interp), 2)
    expect_equal(names(interp), c("superficie", "colina"))

    expect_equal(getcolina.thinplate(interp), colinadummy)

    # PREDICT VAZIO
    gg <- expand.grid(hl = 1:10, pot = NA)
    expect_equal(predict(interp, gg), numeric(0))

    gg <- coordgrade(colinadummy, 20, 20)

    pred_vec <- predict(interp, gg)
    expect_snapshot_value(pred_vec, style = "json2")

    pred_full <- predict(interp, gg, TRUE)
    expect_equal(class(pred_full), "gradecolina")
    expect_equal(class(pred_full[[2]]), "curvacolina")
    expect_equal(colnames(pred_full[[1]]), c("hl", "pot", "rend", "inhull"))
    expect_equal(pred_full[[1]]$rend, pred_vec)

    p3d <- plot(interp, print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p3d), c("plotly", "htmlwidget"))

    p2d <- plot(interp, tipo = "2d", print = FALSE, dhl = 10, dpot = 10)
    expect_equal(class(p2d), c("gg", "ggplot"))
})

test_that("Interolador Multiplo", {

    colina <- colinadummy[rend > 92]
    interp <- interpolador(colina, c("thinplate", "triangulacao"), 95.5, tessfunc = tessradial)

    # primeira metade

    expect_equal(class(interp$superficies[[1]]), c("thinplate", "interpolador"))
    expect_equal(length(interp$superficies[[1]]), 2)
    expect_equal(names(interp$superficies[[1]]), c("superficie", "colina"))

    expect_equal(attr(interp$superficies[[1]]$colina, "rends"), c(93, 93.5, 94, 94.5, 95, 95.5))
    expect_true(is.na(attr(interp$superficies[[1]]$colina, "max")))

    # primeira metade

    expect_equal(class(interp$superficies[[2]]), c("triangulacao", "interpolador"))
    expect_equal(length(interp$superficies[[2]]), 2)
    expect_equal(names(interp$superficies[[2]]), c("triangulos", "colina"))

    expect_equal(attr(interp$superficies[[2]]$colina, "rends"), c(95.5, 95.78))
    expect_equal(attr(interp$superficies[[2]]$colina, "max"), 95.78)
})